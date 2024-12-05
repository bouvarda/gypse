use std::fs;
use std::io::Write;
use std::path::Path;
use log::{error, info};

use crate::cpu::CPU;
use crate::graphics::Graphics;
use crate::memory::PhysicalMemory;
use crate::process::Process;
use crate::utils::Interruption;

pub struct VirtualMachine {
    flags: u8,
    cpu: CPU,
    memory: PhysicalMemory,
    processes: Vec<Process>,
    graphics: Option<Graphics>,
}

impl VirtualMachine {
    pub fn new(vm_flags: u8) -> VirtualMachine {
        VirtualMachine {
            flags: vm_flags,
            cpu: CPU::new(vm_flags),
            memory: PhysicalMemory::new(),
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
        process.load_from_binary(buffer, &mut self.memory)?;
        self.processes.push(process);
        Ok(())
    }

    pub fn run(&mut self) {
        loop {
            let mut can_stop = true;
            for process in &mut self.processes {
                if !process.is_stopped() {
                    can_stop = false;
                    match self.cpu.execute(process, &mut self.memory, Some(1000)) {
                        Ok(_) => {},
                        Err(Interruption::GraphicsInit(w, h)) => {
                            self.graphics = Some(Graphics::new(w as usize, h as usize));
                            process.set_running(); // resume interruption
                        },
                        Err(Interruption::GraphicsUpdate(buffer_addr)) => {
                            match self.graphics.as_mut() {
                                Some(g) => {
                                    if g.is_open() {
                                        g.update(buffer_addr, &mut self.memory, process);
                                        process.set_running(); // resume interruption
                                    } else {
                                        process.stop(); // window is closed, terminate process
                                    }
                                }
                                None => panic!("Graphics not initialized")
                            }
                        },
                        Err(_) => panic!("Virtual machine panicked")
                    }
                }
            }
            if can_stop {
                break;
            }
        }
        info!("Stop virtual machine, it has run {} cycles", self.cpu.cycle_count);
    }
}
