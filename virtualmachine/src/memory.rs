use crate::process::Process;

// 4k physical page frame
const PAGE_SIZE: usize = 4096;

type PhysicalPageFrame = [u8; PAGE_SIZE];

pub struct PhysicalMemory {
    page_frames: Vec<PhysicalPageFrame>,
}

impl PhysicalMemory {
    pub fn new() -> PhysicalMemory {
        PhysicalMemory { page_frames: vec![] }
    }

    pub fn read_u8(&mut self, process: &mut Process, virtual_addr: u32) -> u8 {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        page_frame[page_frame_offset]
    }

    pub fn read_i16(&mut self, process: &mut Process, virtual_addr: u32) -> i16 {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        let b1 = page_frame[page_frame_offset];
        let b2: u8;
        if page_frame_offset + 1 < PAGE_SIZE {
            b2 = page_frame[page_frame_offset+1];
        } else {
            b2 = self.read_u8(process, virtual_addr + 2);
        }
        // little endian
        // b1 | (b2 >> 8)
        i16::from_le_bytes([b1, b2])
    }

    pub fn read_u16(&mut self, process: &mut Process, virtual_addr: u32) -> u16 {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        let b1 = page_frame[page_frame_offset];
        let b2: u8;
        if page_frame_offset + 1 < PAGE_SIZE {
            b2 = page_frame[page_frame_offset+1];
        } else {
            b2 = self.read_u8(process, virtual_addr + 2);
        }
        // little endian
        // b1 | (b2 >> 8)
        u16::from_le_bytes([b1, b2])
    }

    pub fn read_u32(&mut self, process: &mut Process, virtual_addr: u32) -> u32 {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;

        let b1 = page_frame[page_frame_offset];
        let b2: u8;
        let b3: u8;
        let b4: u8;
        if page_frame_offset + 4 < PAGE_SIZE {
            b2 = page_frame[page_frame_offset+1];
            b3 = page_frame[page_frame_offset+2];
            b4 = page_frame[page_frame_offset+3];
        } else {
            // change page
            b2 = self.read_u8(process, virtual_addr + 1);
            b3 = self.read_u8(process, virtual_addr + 2);
            b4 = self.read_u8(process, virtual_addr + 3);
        }
        // little endian
        // b1 | (b2 >> 8) | (b3 >> 16) | (b4 >> 24)
        u32::from_le_bytes([b1, b2, b3, b4])
    }

    pub fn read_word(&mut self, word_size: u8, process: &mut Process, virtual_addr: u32) -> u32 {
        match word_size {
            1 => self.read_u8(process, virtual_addr) as u32,
            2 => self.read_u16(process, virtual_addr) as u32,
            4 => self.read_u32(process, virtual_addr),
            _ => panic!("Unsupported word size: {word_size}")
        }
    }

    pub fn read_string(&mut self, process: &mut Process, virtual_addr: u32) -> String {
        let mut string = String::new();
        let mut i: u32 = 0;
        loop {
            let c = self.read_u8(process, virtual_addr + i);
            if c == 0 {
                break;
            }
            string.push(char::from(c));
            i += 1;
        }
        string
    }

    // Read a byte directly from physical memory without virtual address translation
    pub fn read_u8_physical(&mut self, physical_addr: u32) -> u8 {
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        page_frame[page_frame_offset]
    }

    // Read a u32 directly from physical memory without virtual address translation
    pub fn read_u32_physical(&mut self, physical_addr: u32) -> u32 {
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;

        if page_frame_offset + 4 > PAGE_SIZE {
            // If we're crossing a page boundary, read byte by byte
            let b1 = self.read_u8_physical(physical_addr);
            let b2 = self.read_u8_physical(physical_addr + 1);
            let b3 = self.read_u8_physical(physical_addr + 2);
            let b4 = self.read_u8_physical(physical_addr + 3);
            return u32::from_le_bytes([b1, b2, b3, b4]);
        }

        // Fast path for aligned access within a single page
        let b1 = page_frame[page_frame_offset];
        let b2 = page_frame[page_frame_offset + 1];
        let b3 = page_frame[page_frame_offset + 2];
        let b4 = page_frame[page_frame_offset + 3];
        
        u32::from_le_bytes([b1, b2, b3, b4])
    }

    // Read multiple u32 values efficiently from a contiguous physical memory region
    pub fn read_u32_physical_batch(&mut self, physical_addr: u32, count: usize, out: &mut [u32]) {
        let mut current_addr = physical_addr;
        let mut out_pos = 0;
        
        while out_pos < count {
            let page_frame = self.get_page_frame(current_addr);
            let page_frame_offset = (current_addr & 0xFFF) as usize;
            
            // Calculate how many u32s we can read from this page
            let words_till_page_end = (PAGE_SIZE - page_frame_offset) / 4;
            let words_remaining = count - out_pos;
            let words_to_read = words_till_page_end.min(words_remaining);
            
            // Fast copy for aligned data within the same page
            for i in 0..words_to_read {
                let offset = page_frame_offset + (i * 4);
                out[out_pos + i] = u32::from_le_bytes([
                    page_frame[offset],
                    page_frame[offset + 1],
                    page_frame[offset + 2],
                    page_frame[offset + 3],
                ]);
            }
            
            current_addr += (words_to_read * 4) as u32;
            out_pos += words_to_read;
        }
    }

    #[allow(dead_code)]  // used in tests
    pub fn write_string(&mut self, process: &mut Process, virtual_addr: u32, string: &str) {
        let mut i = 0;
        for b in string.bytes() {
            self.write_u8(process, virtual_addr + i, b);
            i += 1;
        }
        self.write_u8(process, virtual_addr + i, 0u8); // string end
    }

    pub fn write_u8(&mut self, process: &mut Process, virtual_addr: u32, value: u8) {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        page_frame[page_frame_offset] = value;
    }

    pub fn write_u16(&mut self, process: &mut Process, virtual_addr: u32, value: u16) {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;
        if page_frame_offset + 2 > PAGE_SIZE {
            panic!("Unaligned access to u16: {physical_addr}")
        }
        // little endian
        page_frame[page_frame_offset] = (value & 0xFF) as u8;
        let b2 = ((value >> 8) & 0xFF) as u8;
        if page_frame_offset + 1 < PAGE_SIZE {
            page_frame[page_frame_offset + 1] = b2;
        } else {
            self.write_u8(process, virtual_addr + 1, b2);
        }
    }

    pub fn write_u32(&mut self, process: &mut Process, virtual_addr: u32, value: u32) {
        let physical_addr = self.translate_virtual_to_physical(process, virtual_addr);
        let page_frame = self.get_page_frame(physical_addr);
        let page_frame_offset = (physical_addr & 0xFFF) as usize;

        // little endian
        page_frame[page_frame_offset] = (value & 0xFF) as u8;
        let b2 = ((value >> 8) & 0xFF) as u8;
        let b3 = ((value >> 16) & 0xFF) as u8;
        let b4 = ((value >> 24) & 0xFF) as u8;
        if page_frame_offset + 4 < PAGE_SIZE {
            page_frame[page_frame_offset+1] = b2;
            page_frame[page_frame_offset+2] = b3;
            page_frame[page_frame_offset+3] = b4;
        } else {
            self.write_u8(process, virtual_addr + 1, b2);
            self.write_u8(process, virtual_addr + 2, b2);
            self.write_u8(process, virtual_addr + 3, b2);
        }
    }

    pub fn write_word(&mut self, word_size: u8, process: &mut Process, virtual_addr: u32, value: u32) {
        match word_size {
            1 => self.write_u8(process, virtual_addr, value as u8),
            2 => self.write_u16(process, virtual_addr, value as u16),
            4 => self.write_u32(process, virtual_addr, value),
            _ => panic!("Unsupported word size: {word_size}")
        };
    }

    pub fn write_slice(&mut self, process: &mut Process, virtual_addr: u32, value: &[u8]) {
        for (i, b) in value.iter().enumerate() {
            self.write_u8(process, virtual_addr + i as u32, *b);
        }
    }

    pub fn zerofill_u32(&mut self, process: &mut Process, virtual_addr: u32, size: u32) {
        for i in 0..size {
            self.write_u32(process, virtual_addr + i * 4, 0);
        }
    }

    fn translate_virtual_to_physical(&mut self, process: &mut Process, virtual_addr: u32) -> u32 {
        process.paging.translate_virtual_to_physical(virtual_addr).unwrap_or_else(|_| {
            let physical_page_frame = self.alloc_page_frame();
            process.paging.assign_page_frame_mapping(virtual_addr, physical_page_frame);
            process.paging.translate_virtual_to_physical(virtual_addr).expect("Cannot allocate memory")
        })
    }

    pub fn get_page_frame(&mut self, physical_addr: u32) -> &mut PhysicalPageFrame {
        let page_frame_index = physical_addr >> 12;  // Remove the -1 to prevent underflow
        if page_frame_index >= self.page_frames.len() as u32 {
            panic!("Physical page fault {page_frame_index}");
        }
        &mut self.page_frames[page_frame_index as usize]
    }

    fn alloc_page_frame(&mut self) -> u32 {
        let page_frame_num = (self.page_frames.len() as u32) << 12;
        self.page_frames.push([0; PAGE_SIZE]);
        page_frame_num
    }
}

#[cfg(test)]
mod tests {
    use std::io::stdout;
    use crate::memory::PhysicalMemory;
    use crate::process::Process;

    #[test]
    fn test_memory_read_write() {
        let mut memory = PhysicalMemory::new();
        let mut process = Process::new("test", Box::new(stdout()), 0);

        memory.write_u8(&mut process, 0xFF000001, 42);
        assert_eq!(memory.read_u8(&mut process, 0xFF000001), 42);

        memory.write_u32(&mut process, 0xFF000004, 777);
        assert_eq!(memory.read_u32(&mut process, 0xFF000004), 777);

    }

    #[test]
    fn test_memory_read_string() {
        let mut memory = PhysicalMemory::new();
        let mut process = Process::new("test", Box::new(stdout()), 0);

        let string = "test string";
        memory.write_string(&mut process, 0xFF0000AB, string);
        assert_eq!(memory.read_string(&mut process, 0xFF0000AB), string);
    }

    #[test]
    fn test_memory_zerofill() {
        let mut memory = PhysicalMemory::new();
        let mut process = Process::new("test", Box::new(stdout()), 0);
        memory.write_u32(&mut process, 0xFF000000, 123);
        memory.write_u32(&mut process, 0xFF000004, 456);
        memory.write_u32(&mut process, 0xFF000008, 789);
        memory.zerofill_u32(&mut process, 0xFF000000, 2);
        assert_eq!(memory.read_u32(&mut process, 0xFF000000), 0);
        assert_eq!(memory.read_u32(&mut process, 0xFF000004), 0);
        assert_eq!(memory.read_u32(&mut process, 0xFF000008), 789);
    }
}