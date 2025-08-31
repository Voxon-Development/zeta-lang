//! fiber_pool.rs
//! ucontext-based stackful fibers + work-stealing + pluggable zero-cost policies.

#![allow(clippy::needless_return)]
#![allow(clippy::transmute_num_to_bytes)]

use std::alloc::{Allocator, Global, Layout};
use std::cell::UnsafeCell;
use std::ffi::c_void;
use std::mem::MaybeUninit;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::Instant;

use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use once_cell::sync::Lazy;

#[repr(C)]
#[derive(Copy, Clone)]
pub struct UContext(pub libc::ucontext_t);

unsafe impl Send for UContext {}
unsafe impl Sync for UContext {}

const DEFAULT_STACK_SIZE: usize = 2 * 1024; // 2KB stacks as requested

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FiberId(pub u64);

static FIBER_ID_SRC: AtomicU64 = AtomicU64::new(1);
static ACTIVE_FIBERS: AtomicU64 = AtomicU64::new(0);

// ------ Fast coarse time source (ticks = ns since start) ------
fn now_ticks() -> u64 {
    static START: Lazy<Instant> = Lazy::new(Instant::now);
    START.elapsed().as_nanos() as u64
}

// ================= Fiber (lean, no HashMap) ===================

pub struct Fiber {
    pub id: FiberId,

    // scheduler-visible, tiny metadata
    pub heat: AtomicU64,          // activity score (app bumps this)
    pub qos: AtomicU32,           // 0..N class weight (policy-defined)
    pub last_ran: AtomicU64,      // ticks when it last yielded

    pub finished: AtomicBool,

    // Execution context & dedicated stack
    ctx: UnsafeCell<UContext>,
    stack_ptr: *mut u8,
    stack_layout: Layout,

    // Entry
    entry: UnsafeCell<Option<unsafe extern "C" fn(*mut c_void) -> ()>>,
    arg: *mut c_void,
}

unsafe impl Send for Fiber {}
unsafe impl Sync for Fiber {}

impl Fiber {
    pub fn new_with_stack(
        entry: unsafe extern "C" fn(*mut c_void) -> (),
        arg: *mut c_void,
        stack_size: usize,
    ) -> Arc<Self> {
        let stack_size = stack_size.max(DEFAULT_STACK_SIZE);
        let layout = Layout::from_size_align(stack_size, 16).expect("layout");
        let stack_ptr = Global.allocate(layout).expect("stack alloc").as_mut_ptr();

        let fiber = Arc::new(Fiber {
            id: FiberId(FIBER_ID_SRC.fetch_add(1, Ordering::Relaxed)),
            heat: AtomicU64::new(0),
            qos: AtomicU32::new(1),
            last_ran: AtomicU64::new(0),
            finished: AtomicBool::new(false),
            ctx: UnsafeCell::new(UContext(unsafe { MaybeUninit::zeroed().assume_init() })),
            stack_ptr,
            stack_layout: layout,
            entry: UnsafeCell::new(Some(entry)),
            arg,
        });

        unsafe {
            let ctx = &mut (*fiber.ctx.get()).0;
            if libc::getcontext(ctx) != 0 {
                panic!("getcontext failed");
            }
            ctx.uc_stack.ss_sp = stack_ptr as *mut c_void;
            ctx.uc_stack.ss_size = stack_size;
            ctx.uc_stack.ss_flags = 0;
            libc::sigemptyset(&mut ctx.uc_sigmask);
            ctx.uc_link = null_mut();

            extern "C" fn fiber_trampoline(fiber_ptr_low: usize, fiber_ptr_high: usize) {
                let raw = ((fiber_ptr_high as u128) << 64) | (fiber_ptr_low as u128);
                let ptr = raw as usize as *mut Fiber;
                let fiber = unsafe { &*ptr };
                let entry = unsafe { (*fiber.entry.get()).take().unwrap() };
                unsafe { entry(fiber.arg) };
                fiber.finished.store(true, Ordering::Release);
                ACTIVE_FIBERS.fetch_sub(1, Ordering::Relaxed);
                unsafe { fiber_sched::yield_to_scheduler() };
            }

            let fptr = Arc::as_ptr(&fiber) as usize as u128;
            let low = fptr as usize;
            let high = (fptr >> 64) as usize;
            libc::makecontext(
                ctx,
                std::mem::transmute::<extern "C" fn(usize, usize), extern "C" fn()>(fiber_trampoline),
                2,
                low,
                high,
            );
        }

        fiber
    }
}

impl Drop for Fiber {
    fn drop(&mut self) {
        unsafe {
            Global.deallocate(std::ptr::NonNull::new_unchecked(self.stack_ptr), self.stack_layout);
        }
    }
}

#[derive(Clone)]
pub struct FiberHandle(Arc<Fiber>);

impl FiberHandle {
    pub fn id(&self) -> FiberId {
        self.0.id
    }
    pub fn heat(&self) -> u64 {
        self.0.heat.load(Ordering::Relaxed)
    }
    pub fn set_heat(&self, v: u64) {
        self.0.heat.store(v, Ordering::Relaxed)
    }
    pub fn inc_heat(&self, by: u64) {
        self.0.heat.fetch_add(by, Ordering::Relaxed);
    }
    pub fn set_qos(&self, class: u32) {
        self.0.qos.store(class.max(1), Ordering::Relaxed);
    }
    pub fn qos(&self) -> u32 {
        self.0.qos.load(Ordering::Relaxed)
    }
    pub fn is_finished(&self) -> bool {
        self.0.finished.load(Ordering::Acquire)
    }
}

// ================= TLS for yielding =================
thread_local! {
    static TLS_SCHED_CTX: UnsafeCell<*mut libc::ucontext_t> = UnsafeCell::new(null_mut());
    static TLS_FIBER_CTX: UnsafeCell<*mut libc::ucontext_t> = UnsafeCell::new(null_mut());
}

pub mod fiber_sched {
    use super::*;
    // Called by running fibers to yield back to the scheduler
    pub unsafe fn yield_to_scheduler() {
        let mut sched_ctx_ptr: *mut libc::ucontext_t = null_mut();
        let mut fiber_ctx_ptr: *mut libc::ucontext_t = null_mut();
        TLS_SCHED_CTX.with(|p| unsafe { sched_ctx_ptr = *p.get() });
        if sched_ctx_ptr.is_null() {
            return;
        }
        
        TLS_FIBER_CTX.with(|p| unsafe { fiber_ctx_ptr = *p.get() });
        if fiber_ctx_ptr.is_null() {
            return;
        }
       unsafe { libc::swapcontext(fiber_ctx_ptr, sched_ctx_ptr); }
    }
}

// ================= Policies (zero-cost via generics) =================

pub trait Policy {
    /// Called once when a fiber is spawned.
    #[inline(always)]
    fn on_spawn(_f: &Arc<Fiber>) {}

    /// Called every time a fiber returns to the scheduler.
    #[inline(always)]
    fn on_yield(_f: &Arc<Fiber>, _now_ticks: u64) {}

    /// Compute a priority score (higher = better).
    fn score(f: &Arc<Fiber>, now_ticks: u64) -> u64;

    /// Map score → bucket index (0 = highest prio).
    #[inline(always)]
    fn bucket(score: u64) -> u32 {
        const BUCKETS: u32 = 64;
        if score == 0 {
            BUCKETS - 1
        } else {
            // log2 bucketing
            let rank = 63u32.saturating_sub(score.leading_zeros());
            (BUCKETS - 1).saturating_sub(rank.min(BUCKETS - 1))
        }
    }

    /// Keep hot fibers local?
    const PREFER_LOCAL: bool = true;
}

/// FIFO policy: flat score, always lowest bucket (round-robin-ish).
pub struct FifoPolicy;

impl Policy for FifoPolicy {
    #[inline(always)]
    fn score(_: &Arc<Fiber>, _: u64) -> u64 {
        1
    }
    #[inline(always)]
    fn bucket(_: u64) -> u32 {
        BUCKETS as u32 - 1
    }
    const PREFER_LOCAL: bool = true;
}

/// Heat × QoS with decay + aging to prevent starvation.
pub struct HeatQosPolicy;

impl Policy for HeatQosPolicy {
    #[inline(always)]
    fn on_yield(f: &Arc<Fiber>, now: u64) {
        // exponential-ish decay based on elapsed time (bit-shift = rate)
        const DECAY_SHIFT: u32 = 18; // tune to your tick scale
        let last = f.last_ran.load(Ordering::Relaxed);
        let dt = now.wrapping_sub(last);
        if dt > 0 {
            let decay = dt >> DECAY_SHIFT;
            if decay != 0 {
                let mut cur = f.heat.load(Ordering::Relaxed);
                loop {
                    let newv = cur.saturating_sub(decay);
                    if f.heat
                        .compare_exchange_weak(cur, newv, Ordering::Relaxed, Ordering::Relaxed)
                        .is_ok()
                    {
                        break;
                    }
                    cur = f.heat.load(Ordering::Relaxed);
                }
            }
        }
    }

    #[inline(always)]
    fn score(f: &Arc<Fiber>, now: u64) -> u64 {
        let heat = f.heat.load(Ordering::Relaxed);
        let qos = f.qos.load(Ordering::Relaxed).max(1) as u64;
        let last = f.last_ran.load(Ordering::Relaxed);
        let age_boost = (now.wrapping_sub(last) >> 22).saturating_add(1); // tiny aging
        heat.saturating_mul(qos).saturating_add(age_boost)
    }

    #[inline(always)]
    fn bucket(score: u64) -> u32 {
        // log2 bucketing into 64 queues, hottest near 0
        let rank = 63u32.saturating_sub(score.leading_zeros());
        (BUCKETS as u32 - 1).saturating_sub(rank.min((BUCKETS as u32) - 1))
    }

    const PREFER_LOCAL: bool = true;
}

// ================= Priority Buckets =================

use std::mem::MaybeUninit as MU;
use std::collections::VecDeque;

const BUCKETS: usize = 64;

struct Buckets<T> {
    qs: [VecDeque<T>; BUCKETS],
    mask: u64, // bit i set if bucket i non-empty
}

impl<T> Buckets<T> {
    fn new() -> Self {
        // SAFETY: VecDeque<T> is moveable; build an array with MU
        let mut tmp: [MU<VecDeque<T>>; BUCKETS] = unsafe { MU::uninit().assume_init() };
        for i in 0..BUCKETS {
            tmp[i] = MU::new(VecDeque::new());
        }
        let qs = unsafe { std::mem::transmute::<_, [VecDeque<T>; BUCKETS]>(tmp) };
        Self { qs, mask: 0 }
    }
    #[inline(always)]
    fn push(&mut self, idx: u32, item: T) {
        let i = (idx as usize).min(BUCKETS - 1);
        self.qs[i].push_back(item);
        self.mask |= 1u64 << i;
    }
    #[inline(always)]
    fn pop(&mut self) -> Option<T> {
        if self.mask == 0 {
            return None;
        }
        let i = self.mask.trailing_zeros() as usize;
        let v = self.qs[i].pop_front();
        if self.qs[i].is_empty() {
            self.mask &= !(1u64 << i);
        }
        v
    }
}

// ================= Work-stealing thread pool (generic over Policy) =================

#[derive(Clone, Copy)]
pub struct FiberPoolOptions {
    pub workers: usize,
    pub stack_size: usize, // default 2KB
}

impl Default for FiberPoolOptions {
    fn default() -> Self {
        let workers = thread::available_parallelism().map(|n| n.get()).unwrap_or(1);
        Self { workers, stack_size: DEFAULT_STACK_SIZE }
    }
}

struct WorkerState<P: Policy> {
    _id: usize,
    sched_ctx: UnsafeCell<UContext>,
    local_buckets: UnsafeCell<Buckets<Arc<Fiber>>>, // policy-priority local queues
    local_fifo: Worker<Arc<Fiber>>,                 // staging lane (crossbeam)
    stealers: Vec<Stealer<Arc<Fiber>>>,
    injector: Arc<Injector<Arc<Fiber>>>,
    opts: FiberPoolOptions,
    stop: Arc<AtomicBool>,
    _marker: std::marker::PhantomData<P>,
}

unsafe impl<P: Policy> Send for WorkerState<P> {}
unsafe impl<P: Policy> Sync for WorkerState<P> {}

pub struct FiberPool<P: Policy> {
    injector: Arc<Injector<Arc<Fiber>>>,
    workers: Vec<Arc<WorkerState<P>>>,
    handles: Vec<JoinHandle<()>>, // only used on Drop
    stop: Arc<AtomicBool>,
    _marker: std::marker::PhantomData<P>,
}

impl<P: Policy + 'static> FiberPool<P> {
    pub fn new(mut opts: FiberPoolOptions) -> Self {
        if opts.workers == 0 {
            opts.workers = 1;
        }
        let injector = Arc::new(Injector::new());
        let stop = Arc::new(AtomicBool::new(false));

        // Build local deques and stealers
        let locals: Vec<Worker<Arc<Fiber>>> = (0..opts.workers).map(|_| Worker::new_fifo()).collect();
        let all_stealers: Vec<Stealer<Arc<Fiber>>> = locals.iter().map(|w| w.stealer()).collect();

        // Worker states
        let mut workers: Vec<Arc<WorkerState<P>>> = Vec::with_capacity(opts.workers);
        for (id, local) in locals.into_iter().enumerate() {
            workers.push(Arc::new(WorkerState::<P> {
                _id: id,
                sched_ctx: UnsafeCell::new(UContext(unsafe { MaybeUninit::zeroed().assume_init() })),
                local_buckets: UnsafeCell::new(Buckets::new()),
                local_fifo: local,
                stealers: all_stealers.clone(),
                injector: injector.clone(),
                opts,
                stop: stop.clone(),
                _marker: std::marker::PhantomData,
            }));
        }

        // Spawn persistent threads
        let mut handles = Vec::with_capacity(workers.len());
        for ws in &workers {
            let ws = ws.clone();
            handles.push(thread::spawn(move || worker_loop::<P>(ws)));
        }

        FiberPool { injector, workers, handles, stop, _marker: std::marker::PhantomData }
    }

    pub fn spawn(&self, entry: unsafe extern "C" fn(*mut c_void) -> (), arg: *mut c_void) -> FiberHandle {
        let stack_size = self.workers.first().map(|w| w.opts.stack_size).unwrap_or(DEFAULT_STACK_SIZE);
        let fiber = Fiber::new_with_stack(entry, arg, stack_size);
        P::on_spawn(&fiber);
        ACTIVE_FIBERS.fetch_add(1, Ordering::Relaxed);
        let handle = FiberHandle(fiber.clone());
        self.injector.push(fiber);
        handle
    }

    /// Block until no active fibers and injector empty.
    pub fn wait_quiescent(&self) {
        loop {
            if ACTIVE_FIBERS.load(Ordering::Relaxed) == 0 && self.injector.is_empty() {
                break;
            }
            thread::yield_now();
        }
    }
}

impl<P: Policy> Drop for FiberPool<P> {
    fn drop(&mut self) {
        self.stop.store(true, Ordering::Release);
        for h in self.handles.drain(..) {
            let _ = h.join();
        }
    }
}

fn worker_loop<P: Policy>(ws: Arc<WorkerState<P>>) {
    unsafe { libc::getcontext(&mut (*ws.sched_ctx.get()).0) };

    loop {
        if ws.stop.load(Ordering::Acquire) {
            break;
        }

        // 1) Prefer local priority buckets
        let job = unsafe { (&mut *ws.local_buckets.get()).pop() }
            // 2) Else try local FIFO / injector / steal, then classify by policy
            .or_else(|| ws.local_fifo.pop())
            .or_else(|| match ws.injector.steal_batch_and_pop(&ws.local_fifo) {
                Steal::Success(f) => Some(f),
                _ => None,
            })
            .or_else(|| {
                for stealer in &ws.stealers {
                    if let Steal::Success(f) = stealer.steal() {
                        return Some(f);
                    }
                }
                None
            });

        let Some(fiber) = job else {
            if ws.stop.load(Ordering::Acquire) {
                break;
            }
            thread::yield_now();
            continue;
        };

        if fiber.finished.load(Ordering::Acquire) {
            continue;
        }

        // Install TLS context pointers for yielding
        TLS_SCHED_CTX.with(|p| unsafe { *p.get() = &mut (*ws.sched_ctx.get()).0 });
        TLS_FIBER_CTX.with(|p| unsafe { *p.get() = &mut (*fiber.ctx.get()).0 });

        // Swap into the fiber; when it yields/finishes, resume here
        unsafe {
            let sched_ctx = &mut (*ws.sched_ctx.get()).0 as *mut _;
            let fiber_ctx = &mut (*fiber.ctx.get()).0 as *mut _;
            if libc::swapcontext(sched_ctx, fiber_ctx) != 0 {
                panic!("swapcontext failed");
            }
        }

        if fiber.finished.load(Ordering::Acquire) {
            // ACTIVE_FIBERS was decremented in trampoline
            continue;
        }

        // Re-enqueue via policy
        let now = now_ticks();
        P::on_yield(&fiber, now);
        fiber.last_ran.store(now, Ordering::Relaxed);

        let score = P::score(&fiber, now);
        let b = P::bucket(score);

        unsafe { (&mut *ws.local_buckets.get()).push(b, fiber) };
    }
}

// ============== Convenience constructors / aliases ==============

pub fn default_fifo_scheduler() -> Arc<FiberPool<FifoPolicy>> {
    Arc::new(FiberPool::<FifoPolicy>::new(FiberPoolOptions::default()))
}

pub fn heat_scheduler() -> Arc<FiberPool<HeatQosPolicy>> {
    Arc::new(FiberPool::<HeatQosPolicy>::new(FiberPoolOptions::default()))
}

// =================== Example usage (optional) ===================
// unsafe extern "C" fn my_task(_arg: *mut c_void) -> () {
//     // ... do work, then yield periodically:
//     unsafe { fiber_sched::yield_to_scheduler() };
// }
//
// fn main() {
//     let pool = heat_scheduler();
//     let _h = pool.spawn(my_task, std::ptr::null_mut());
//     pool.wait_quiescent();
// }
