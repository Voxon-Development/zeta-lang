use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicI32, Ordering};
use libc::{syscall, SYS_futex, FUTEX_PRIVATE_FLAG, FUTEX_WAIT, FUTEX_WAKE};
use trc::SharedTrc;

/// Allows for mutable access to the inner value of an UnsafeArc without
/// having to clone the arc or acquire a lock from a Mutex.
/// While it is not the safest because it is made to not acquire a lock,
/// it is the most performant because you don't need synchronization or mutexes.
///
/// # Safety
/// You must ensure no data races occur (e.g. via manual synchronization or thread safety on the element).
pub struct UnsafeArc<T> {
    inner: SharedTrc<UnsafeCell<T>>,
}

unsafe impl<T> Send for UnsafeArc<T> where T: Send {}
unsafe impl<T> Sync for UnsafeArc<T> where T: Send {}

impl<T> Clone for UnsafeArc<T> {
    fn clone(&self) -> Self {
        UnsafeArc {
            inner: SharedTrc::clone(&self.inner),
        }
    }
}

impl<T> UnsafeArc<T> {
    pub fn new(value: T) -> Self {
        Self {
            inner: SharedTrc::new(UnsafeCell::new(value)),
        }
    }

    /// # Safety
    /// You must ensure no data races occur (e.g. via manual synchronization).
    #[allow(clippy::mut_from_ref)]
    pub unsafe fn get_mut_unchecked(&self) -> &mut T {
        unsafe { &mut *self.inner.get() }
    }
}

/// A raw, minimal futex-based lock.
/// This is faster than `std::sync::Mutex` for uncontended paths.
/// It does **not** implement poisoning, fairness, or reentrancy.
#[derive(Default)]
pub struct Futex {
    state: AtomicI32,
}

impl Futex {
    /// Creates a new, unlocked futex.
    pub const fn new() -> Self {
        Self {
            state: AtomicI32::new(0),
        }
    }

    /// Acquires the lock, blocking the current thread if necessary.
    pub fn lock(&self) {
        // Fast path: try to acquire the lock immediately.
        while self
            .state
            .compare_exchange(0, 1, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            // Slow path: wait until the lock is available.
            unsafe {
                syscall(SYS_futex, &self.state as *const _, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, 1);
            }
        }
    }

    /// Releases the lock and wakes one waiting thread.
    pub fn unlock(&self) {
        self.state.store(0, Ordering::Release);
        unsafe {
            syscall(
                SYS_futex,
                &self.state as *const _,
                FUTEX_WAKE | FUTEX_PRIVATE_FLAG,
                1,
            );
        }
    }
}

unsafe impl Send for Futex {}
unsafe impl Sync for Futex {}