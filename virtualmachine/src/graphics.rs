use minifb::{Key, Window, WindowOptions};

use crate::{memory::PhysicalMemory, process::Process};

pub struct Graphics {
    width: usize,
    height: usize,
    window: Window,
    framebuffer: Vec<u32>,
}

impl Graphics {
    pub fn new(width: usize, height: usize) -> Graphics {
        let window = Window::new(
            "Gypse",
            width,
            height,
            WindowOptions::default(),
        )
        .unwrap_or_else(|e| { 
            panic!("Cannot init graphics: {}", e);
        });
        let framebuffer = vec![0; width * height];
        Graphics {
            width, height, window, framebuffer
        }
    }

    pub fn is_open(&self) -> bool {
        self.window.is_open() && !self.window.is_key_down(Key::Escape)
    }

    pub fn update(&mut self, buffer_virtual_addr: u32, memory: &mut PhysicalMemory, process: &mut Process) {
        if self.is_open() {
            // copy buffer from virtual process memory to the window framebuffer
            for (i, pixel) in self.framebuffer.iter_mut().enumerate() {
                *pixel = memory.read_u32(process, buffer_virtual_addr + i as u32 * 4);
            } 
            self.window.update_with_buffer(&self.framebuffer, self.width, self.height)
                .unwrap_or_else(|e| { 
                    panic!("Cannot update graphics: {}", e);
                })
        }
    }
}