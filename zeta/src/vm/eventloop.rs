// async version of Vec
use std::sync::Arc;
use std::sync::Mutex;

struct EventLoop {
    events: Arc<Mutex<Vec<Event>>>,
    event_queue: Arc<Mutex<Vec<Event>>>
}

impl EventLoop {
    fn new() -> EventLoop {
        EventLoop {
            events: Vec::new(),
            event_queue: Vec::new(),
        }
    }

    async fn add_event(&mut self, event: Event) {
        self.event_queue.lock().unwrap().push(event);
    }

    async fn process_events(&mut self) {
        let mut events_queue = self.events_queue.lock().unwrap();
        let mut events = self.events.lock().unwrap();
        for event in events_queue.iter() {
            events.push(event.clone());
        }
        events_queue.clear();
    }
}