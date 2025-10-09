use std::time::{Duration, Instant};
use log::info;

pub struct PerformanceMetrics {
    frame_times: Vec<Duration>,
    last_frame: Instant,
    current_batch_start: Instant,
    batch_size: usize,
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        PerformanceMetrics {
            frame_times: Vec::with_capacity(60),
            last_frame: Instant::now(),
            current_batch_start: Instant::now(),
            batch_size: 60,  // Measure over 60 frames
        }
    }

    pub fn start_frame(&mut self) {
        self.last_frame = Instant::now();
    }

    pub fn end_frame(&mut self) {
        let frame_time = self.last_frame.elapsed();
        self.frame_times.push(frame_time);

        if self.frame_times.len() >= self.batch_size {
            self.report_metrics();
            self.frame_times.clear();
        }
    }

    pub fn report_metrics(&self) {
        if self.frame_times.is_empty() {
            return;
        }

        let total_time: Duration = self.frame_times.iter().sum();
        let avg_frame_time = total_time / self.frame_times.len() as u32;
        
        let mut sorted_times = self.frame_times.clone();
        sorted_times.sort();
        
        let percentile_99 = sorted_times[((self.frame_times.len() - 1) * 99) / 100];
        let max_time = sorted_times.last().unwrap();
        
        let fps = 1.0 / avg_frame_time.as_secs_f64();
        
        info!(
            "Graphics Performance Metrics:\n\
             Average frame time: {:.2?}\n\
             99th percentile: {:.2?}\n\
             Max frame time: {:.2?}\n\
             Estimated FPS: {:.1}\n\
             Frames measured: {}",
            avg_frame_time,
            percentile_99,
            max_time,
            fps,
            self.frame_times.len()
        );
    }

    pub fn start_batch(&mut self) {
        self.current_batch_start = Instant::now();
    }

    pub fn end_batch(&self, pixels_processed: usize) {
        let duration = self.current_batch_start.elapsed();
        let pixels_per_second = pixels_processed as f64 / duration.as_secs_f64();
        info!(
            "Batch Performance:\n\
             Time: {:.2?}\n\
             Pixels: {}\n\
             Pixels/second: {:.2e}",
            duration,
            pixels_processed,
            pixels_per_second
        );
    }
}