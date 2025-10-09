use std::fs;
use std::io::Write;
use std::path::Path;
use log::{error, info};

use std::sync::{Arc, Mutex};
use crate::cpu::CPU;
use crate::graphics_thread::GraphicsThread;
use crate::memory::PhysicalMemory;
use crate::process::Process;
use crate::utils::Interruption;

pub struct VirtualMachine {
    flags: u8,
    cpu: CPU,
    memory: Arc<Mutex<PhysicalMemory>>,
    processes: Vec<Process>,
    graphics: Option<GraphicsThread>,
}

impl VirtualMachine {
    pub fn new(vm_flags: u8) -> VirtualMachine {
        VirtualMachine {
            flags: vm_flags,
            cpu: CPU::new(vm_flags),
            memory: Arc::new(Mutex::new(PhysicalMemory::new())),
            processes: vec![],
            graphics: None,
        }
    }

    pub fn load_process(&mut self, file_path: &Path, stdout: Box<dyn Write>) -> Result<(), Interruption> {
        let buffer = fs::read(file_path)
            .map_err(|e| {
                error!("Cannot open process {}: {}", file_path.to_str().unwrap(), e);
                Interruption::KernelPanic
            })?;
        info!("Load executable file {:?}", file_path.as_os_str());

        let mut process = Process::new(file_path.file_name().unwrap().to_str().unwrap(), stdout, self.flags);
        
        // Lock memory for process loading
        if let Ok(mut memory) = self.memory.lock() {
            process.load_from_binary(buffer, &mut memory)?;
            self.processes.push(process);
            Ok(())
        } else {
            Err(Interruption::KernelPanic)
        }
    }

    pub fn run(&mut self) {
        loop {
            let mut can_stop = true;
            for process in &mut self.processes {
                if !process.is_stopped() {
                    can_stop = false;
                    // Execute CPU in larger batches for better performance while still allowing graphics updates
                    if let Ok(mut memory) = self.memory.lock() {
                        match self.cpu.execute(process, &mut memory, Some(1000)) {
                            Ok(_) => {},
                            Err(Interruption::GraphicsInit(w, h, buffer)) => {
                                self.graphics = Some(GraphicsThread::new(w as usize, h as usize, buffer, self.memory.clone()));
                                process.set_running(); // resume interruption
                            },
                            Err(_) => panic!("Virtual machine panicked")
                        }
                    }
                    // Brief yield to allow graphics thread to run
                    std::thread::yield_now();
                }
            }
            if can_stop {
                break;
            }
        }
        // Shutdown graphics thread if it exists
        if let Some(graphics) = self.graphics.take() {
            graphics.shutdown();
        }
        info!("Stop virtual machine, it has run {} cycles", self.cpu.cycle_count);
    }
}
