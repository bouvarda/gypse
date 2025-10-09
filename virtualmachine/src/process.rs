use std::io::Write;
use common::EXEC_DEBUG_FLAG;
use log::error;
use crate::debugger::Debugger;
use crate::heap::Heap;
use crate::memory::PhysicalMemory;
use crate::paging::Paging;
use crate::utils::Interruption;

#[derive(PartialEq, Debug, Clone)]
pub enum ProcessState {
    CREATED,
    RUNNING,
    SUSPENDED,
    STOPPED,
}

pub struct Process {
    pub name: String,
    pub state: ProcessState,
    pub paging: Paging,
    pub heap: Heap,

    pub text_entry_addr: u32,
    pub text_segment_addr: u32,
    pub data_segment_addr: u32,
    pub heap_segment_addr: u32,
    pub debug_segment_addr: Option<u32>,

    pub stdout: Box<dyn Write>,
    pub debugger: Debugger,
}

const BIN_VERSION: u8 = 2;

impl Process {
    pub fn new(name: &str, stdout: Box<dyn Write>, vm_flags: u8) -> Process {
        Process {
            name: String::from(name),
            state: ProcessState::CREATED,
            paging: Paging::new(),
            heap: Heap::empty(),
            text_entry_addr: 0x00000000,
            text_segment_addr: 0x00000000,
            data_segment_addr: 0x00000000,
            heap_segment_addr: 0x00000000,
            debug_segment_addr: None,
            stdout,
            debugger: Debugger::new(vm_flags),
        }
    }

    pub fn is_created(&self) -> bool {
        self.state == ProcessState::CREATED
    }

    pub fn is_running(&self) -> bool {
        self.state == ProcessState::RUNNING
    }

    pub fn is_stopped(&self) -> bool {
        self.state == ProcessState::STOPPED
    }

    pub fn suspend(&mut self) {
        self.state = ProcessState::SUSPENDED;
    }

    pub fn set_running(&mut self) {
        self.state = ProcessState::RUNNING;
    }

    pub fn stop(&mut self) {
        self.state = ProcessState::STOPPED;
    }

    pub fn load_from_binary(&mut self, buffer: Vec<u8>, memory: &mut PhysicalMemory) -> Result<(), Interruption> {
        #[repr(C)]
        struct ExecutableHeader {
            magic: [u8; 4],
            version: u8,
            flags: u8,
            entry: u32,
            data_header: SectionHeader,
            text_header: SectionHeader,
            debug_header: SectionHeader,
        }

        #[repr(C)]
        struct SectionHeader {
            offset: u32,
            size: u32,
        }

        let (head, body, _tail) = unsafe { buffer.align_to::<ExecutableHeader>() };
        if !head.is_empty() {
            error!("Executable header is not aligned");
            return Err(Interruption::KernelPanic);
        }
        let header = &body[0];
        if header.magic != [0x43, 0x4f, 0x4f, 0x4c] {
            error!("Invalid Executable magic");
            return Err(Interruption::KernelPanic);
        }
        if header.version != BIN_VERSION {
            error!("Unsupported Executable version");
            return Err(Interruption::KernelPanic);
        }
        self.text_segment_addr = 0x00000000;
        self.data_segment_addr = header.text_header.size;

        self.load_segment_into_memory(&buffer, header.text_header.offset, header.text_header.size, memory, self.text_segment_addr);
        self.load_segment_into_memory(&buffer, header.data_header.offset, header.data_header.size, memory, self.data_segment_addr);

        if header.flags & EXEC_DEBUG_FLAG == 1 {
            self.debug_segment_addr = Some(self.data_segment_addr + header.data_header.size);
            self.load_segment_into_memory(&buffer, header.debug_header.offset, header.debug_header.size, memory, self.debug_segment_addr.unwrap());
        }

        self.heap_segment_addr = self.data_segment_addr + header.data_header.size + header.debug_header.size;
        self.heap = Heap::new(self.heap_segment_addr, 0xffff0000); // reserve space for the stack at high memory addresses

        self.text_entry_addr = header.entry;
        Ok(())
    }

    fn load_segment_into_memory(&mut self, buffer: &Vec<u8>, offset: u32, size: u32, memory: &mut PhysicalMemory, segment_virtual_addr: u32) {
        let segment_start = offset as usize;
        let segment_end = segment_start + size as usize;
        memory.write_slice(self, segment_virtual_addr, &buffer[segment_start..segment_end]);
    }

    // Get the physical address for a virtual address
    pub fn get_physical_addr(&mut self, virtual_addr: u32) -> Result<u32, Interruption> {
        self.paging.translate_virtual_to_physical(virtual_addr)
            .map_err(|_| Interruption::ProcessFault)
    }
}