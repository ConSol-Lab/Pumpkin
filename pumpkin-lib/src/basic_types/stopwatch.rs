use std::time::Instant;

pub struct Stopwatch {
    time_limit_in_seconds: i64,
    time_start: Instant,
}

impl Stopwatch {
    pub fn new(time_limit_in_seconds: i64) -> Stopwatch {
        Stopwatch {
            time_limit_in_seconds,
            time_start: Instant::now(),
        }
    }

    pub fn reset(&mut self, time_limit_in_seconds: i64) {
        self.time_limit_in_seconds = time_limit_in_seconds;
        self.time_start = Instant::now();
    }

    pub fn get_elapsed_time(&self) -> u64 {
        self.time_start.elapsed().as_secs()
    }

    pub fn get_remaining_time_budget(&self) -> i64 {
        self.time_limit_in_seconds - self.get_elapsed_time() as i64
    }
}
