use std::fs::File;
use std::io::Write;
use crate::memory::PhysicalMemory;
use crate::process::Process;

#[derive(Debug)]
pub enum Interruption {
    ProcessFault,
    ProcessTermination,
    GraphicsInit(u32, u32, u32),
    KernelPanic,
}

pub fn print_stack(process: &mut Process, memory: &mut PhysicalMemory, stack_pointer: u32, stack_base_pointer: u32) {
    println!("=== Stack bottom ===");
    let mut addr: u32 = 0xFFFFFFF0;
    while addr >= stack_pointer {
        let is_base = if addr == stack_base_pointer { "<==== SB"} else { "" };
        let value = memory.read_u32(process, addr);
        println!("{:#x}[{}]: {} [{}] {}", addr, addr as i32, value, value as i32, is_base);
        addr -= 4;
    }
    println!("=== Stack top ===");
}

pub fn print_memory(process: &mut Process, memory: &mut PhysicalMemory, addr: u32) {
    let value = memory.read_u32(process, addr);
    println!("{:#x}[{}]: {} [{}]", addr, addr as i32, value, value as i32);
}

pub fn print_heap(process: &mut Process, memory: &mut PhysicalMemory) {
    println!("=== Heap end ===");
    let mut high = process.heap.highest_allocated_addr();
    let low = process.heap_segment_addr;
    while high >= low {
        print_memory(process, memory, high);
        high -= 4;
    }
    println!("=== Heap start ===");
}

pub fn print_data(process: &mut Process, memory: &mut PhysicalMemory) {
    println!("=== Data start ===");
    let mut low = process.data_segment_addr;
    let high = process.debug_segment_addr.unwrap_or(process.heap_segment_addr);
    while low < high {
        print_memory(process, memory, low);
        low += 4;
    }
    println!("=== Data end ===");
}

pub fn dump_heap(process: &mut Process, memory: &mut PhysicalMemory) {
    let high = process.heap.highest_allocated_addr();
    let mut low = process.heap_segment_addr;
    let mut dump_file: File = File::create("dump.bin").expect("cannot create dump file");
    while high >= low {
        let value = memory.read_u8(process, low);
        dump_file.write(&[value]).expect("cannot write to dump file");
        low += 1;
    }
}