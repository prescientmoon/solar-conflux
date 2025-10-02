use std::sync::{Arc, Mutex};

#[salsa::db]
#[derive(Clone)]
pub struct TheDatabaseImpl {
	storage: salsa::Storage<Self>,

	// The logs are only used for testing and demonstrating reuse:
	#[allow(unused)]
	logs: Arc<Mutex<Option<Vec<String>>>>,
}

#[salsa::db]
impl salsa::Database for TheDatabaseImpl {}

#[cfg(test)]
impl Default for TheDatabaseImpl {
	fn default() -> Self {
		let logs = <Arc<Mutex<Option<Vec<String>>>>>::default();
		Self {
			storage: salsa::Storage::new(Some(Box::new({
				let logs = logs.clone();
				move |event| {
					eprintln!("Event: {event:?}");
					// Log interesting events, if logging is enabled
					if let Some(logs) = &mut *logs.lock().unwrap() {
						// only log interesting events
						if let salsa::EventKind::WillExecute { .. } = event.kind {
							logs.push(format!("Event: {event:?}"));
						}
					}
				}
			}))),
			logs,
		}
	}
}
