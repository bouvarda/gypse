use std::sync::{Arc, Mutex};
use std::sync::mpsc::{Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

use minifb::{Key, Window, WindowOptions};
use log::info;

use crate::memory::PhysicalMemory;
use crate::metrics::PerformanceMetrics;

const BATCH_SIZE: usize = 4096; // 4K pixels per batch as specified in OPTIM.md

pub struct GraphicsThread {
    width: usize,
    height: usize,
    buffer_addr: u32,
    memory: Arc<Mutex<PhysicalMemory>>,
    thread_handle: Option<thread::JoinHandle<()>>,
}

impl GraphicsThread {
    pub fn new(width: usize, height: usize, buffer_addr: u32, memory: Arc<Mutex<PhysicalMemory>>) -> GraphicsThread {
        let thread_handle = Some(Self::spawn_graphics_thread(width, height, buffer_addr, memory.clone()));

        GraphicsThread {
            width,
            height,
            buffer_addr,
            memory,
            thread_handle,
        }
    }

    fn spawn_graphics_thread(
        width: usize,
        height: usize,
        buffer_addr: u32,
        memory: Arc<Mutex<PhysicalMemory>>,
    ) -> thread::JoinHandle<()> {
        thread::spawn(move || {
            let mut window = Window::new(
                "Gypse",
                width,
                height,
                WindowOptions::default(),
            ).unwrap_or_else(|e| {
                panic!("Cannot init graphics: {}", e);
            });

            let total_pixels = width * height;
            let mut front_buffer = vec![0; total_pixels];
            let mut back_buffer = vec![0; total_pixels];
            let frame_duration = Duration::from_secs_f64(1.0 / 60.0); // 60 FPS target
            let mut last_update = Instant::now();
            let mut metrics = PerformanceMetrics::new();

            info!("Graphics thread started with {}x{} resolution", width, height);

            while window.is_open() && !window.is_key_down(Key::Escape) {
                
                //metrics.start_batch();
                for batch_start in (0..total_pixels).step_by(BATCH_SIZE) {
                    let batch_size = BATCH_SIZE.min(total_pixels - batch_start);
                    let mut batch = vec![0u32; batch_size];
                    
                    // Minimize lock scope
                    if let Ok(mut memory) = memory.lock() {
                        for i in 0..batch_size {
                            let addr = buffer_addr + ((batch_start + i) * 4) as u32;
                            batch[i] = if addr % 4 == 0 {
                                memory.read_u32_physical(addr)
                            } else {
                                // Handle unaligned addresses
                                let b1 = memory.read_u8_physical(addr);
                                let b2 = memory.read_u8_physical(addr + 1);
                                let b3 = memory.read_u8_physical(addr + 2);
                                let b4 = memory.read_u8_physical(addr + 3);
                                u32::from_le_bytes([b1, b2, b3, b4])
                            };
                        }
                    }
                    
                    // Copy batch to back buffer
                    back_buffer[batch_start..batch_start + batch_size].copy_from_slice(&batch);
                }
                
                //metrics.end_batch(total_pixels);

                // Update at fixed rate
                let now = Instant::now();
                if now.duration_since(last_update) >= frame_duration {
                    // Swap buffers
                    std::mem::swap(&mut front_buffer, &mut back_buffer);
                    
                    metrics.start_frame();
                    window.update_with_buffer(&front_buffer, width, height)
                        .unwrap_or_else(|e| {
                            panic!("Cannot update graphics: {}", e);
                        });
                    metrics.end_frame();
                    
                    last_update = now;
                } else {
                    // Sleep to avoid busy waiting while maintaining target frame rate
                    let sleep_duration = frame_duration
                        .checked_sub(now.duration_since(last_update))
                        .unwrap_or(Duration::from_millis(1));
                    thread::sleep(sleep_duration);
                }
            }
        })
    }

    pub fn shutdown(mut self) {
        if let Some(handle) = self.thread_handle.take() {
            let _ = handle.join();
        }
    }
}