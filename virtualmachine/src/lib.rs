use std::io::Write;
use std::path::Path;
use crate::virtualmachine::VirtualMachine;

mod virtualmachine;
mod process;
mod memory;
mod paging;
mod utils;
mod cpu;
mod syscall;
mod heap;
mod debugger;
mod graphics;

pub const FLAG_ENABLE_DEBUGGER: u8 = 1;
pub const FLAG_ENABLE_TRACE: u8 = 2;

pub fn run(bin_path: &Path, stdout: Box<dyn Write>, vm_flags: u8) {
    let mut vm = VirtualMachine::new(vm_flags);
    match vm.load_process(bin_path, stdout) {
        Ok(_) => {}
        Err(_) => panic!("ERROR: Cannot load program"),
    }
    vm.run();
}
