use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
struct HeapBlock {
    base_addr: u32,
    size: u32,
    available: bool,
    prev: Option<u32>,
    next: Option<u32>,
}

pub struct Heap {
    blocks: HashMap<u32, HeapBlock>,
}

impl Heap {

    pub fn empty() -> Heap {
        Heap { blocks: HashMap::new() }
    }

    pub fn new(start_addr: u32, end_addr: u32) -> Heap {
        let first_block = HeapBlock {
            base_addr: start_addr,
            size: end_addr - start_addr,
            available: true,
            prev: None,
            next: None
        };
        let mut blocks = HashMap::new();
        blocks.insert(start_addr, first_block);
        Heap {
            blocks,
        }
    }

    pub fn malloc(&mut self, requested_size: u32) -> Option<u32> {
        let mut allocated_addr: Option<u32> = None;
        let mut free_block_to_insert: Option<HeapBlock> = None;

        // search for a free block that is available and large enough
        for block in self.blocks.values() {
            if block.available && block.size > requested_size {
                if block.size != requested_size {
                    let remaining = block.size - requested_size;
                    free_block_to_insert = Some(HeapBlock {
                        base_addr: block.base_addr + requested_size,
                        size: remaining,
                        available: true,
                        prev: Some(block.base_addr),
                        next: block.next,
                    });
                }
                allocated_addr = Some(block.base_addr);
                break;
            }
        }
        // update the blocks outside of the search loop to avoid double mutable borrows
        match allocated_addr {
            Some(addr) => {
                let block: &mut HeapBlock = self.blocks.get_mut(&addr).unwrap();
                block.available = false;
                block.size = requested_size;

                match free_block_to_insert {
                    Some(free_block) => {
                        let next_addr = free_block.base_addr;
                        block.next = Some(next_addr);
                        self.blocks.insert(next_addr, free_block);
                    }
                    None => {}
                }
                Some(addr)
            },
            None => None,
        }
    }

    pub fn free(&mut self, addr: u32) {
        if self.blocks.contains_key(&addr) {
            let prev: Option<u32>;
            let next: Option<u32>;
            let size: u32;
            // update the block as available (in a limited scope to release the borrowed ref)
            {
                let block: &mut HeapBlock = self.blocks.get_mut(&addr).unwrap();
                block.available = true;
                prev = block.prev;
                next = block.next;
                size = block.size;
            }
            // try to merge freed block with previous one or next one  (TODO: merge both previous and next)
            if prev.is_some() {
                let prev_addr: u32 = prev.unwrap();
                let prev_block: &mut HeapBlock = self.blocks.get_mut(&prev_addr).unwrap();
                if prev_block.available {
                    prev_block.size += size;
                    prev_block.next = Some(prev_addr);
                    prev_block.next = next;
                    if next.is_some() {
                        self.update_prev(next.unwrap(), prev_addr);
                    }
                    self.blocks.remove(&addr);
                }
            } else if next.is_some() {
                let next_addr: u32 = next.unwrap();
                let next_block: &HeapBlock = self.blocks.get(&next_addr).unwrap();
                let next_block_size: u32 = next_block.size;
                let next_block_next: Option<u32> = next_block.next;
                if next_block.available {
                    let current_block: &mut HeapBlock = self.blocks.get_mut(&addr).unwrap();
                    current_block.size += next_block_size;
                    current_block.next = next_block_next;
                    if next_block_next.is_some() {
                        self.update_prev(next_block_next.unwrap(), addr);
                    }
                    self.blocks.remove(&next_addr);
                }
            }
        } else {
            panic!("Cannot free unallocated address {addr}")
        }
    }

    pub fn highest_allocated_addr(&self) -> u32 {
        let mut highest = 0;
        for (addr, block) in &self.blocks {
            if !block.available && addr + block.size > highest {
                highest = addr + block.size - 4 // TODO
            }
        }
        highest
    }

    fn update_prev(&mut self, addr: u32, prev_addr: u32) {
        let next_block: &mut HeapBlock = self.blocks.get_mut(&addr).unwrap();
        next_block.prev = Some(prev_addr);
    }
}

#[cfg(test)]
mod tests {
    use crate::heap::Heap;

    #[test]
    fn test_malloc_simple() {
        let mut heap = Heap::new(0, 4096);
        let addr = heap.malloc(100).unwrap();
        assert_eq!(addr, 0);
        assert_eq!(heap.blocks.len(), 2);
        assert_eq!(heap.blocks[&addr].available, false);
        assert_eq!(heap.blocks[&addr].size, 100);
        assert_eq!(heap.blocks[&addr].prev, None);
        assert_eq!(heap.blocks[&addr].next, Some(100));
        let next_addr = 100;
        assert_eq!(heap.blocks[&next_addr].available, true);
        assert_eq!(heap.blocks[&next_addr].size, 4096-100);
        assert_eq!(heap.blocks[&next_addr].prev, Some(0));
        assert_eq!(heap.blocks[&next_addr].next, None);
    }

    #[test]
    fn test_free_simple() {
        let mut heap = Heap::new(0, 4096);
        let addr = heap.malloc(100).unwrap();
        heap.free(addr);
        assert_eq!(heap.blocks.len(), 1);
        assert_eq!(heap.blocks[&addr].available, true);
        assert_eq!(heap.blocks[&addr].size, 4096);
        assert_eq!(heap.blocks[&addr].prev, None);
        assert_eq!(heap.blocks[&addr].next, None);
    }

    fn init_heap_fragmented() -> Heap {
        // [0 - 511]: used, [512 - 1023]: free, [1024, 1535]: used, [1536 - 4096]: free
        let mut heap = Heap::new(0, 4096);
        for _ in 0..3 {
            heap.malloc(512);
        }
        heap.free(512);
        assert_eq!(heap.blocks.len(), 4);
        assert_eq!(heap.blocks[&512].available, true);
        heap
    }

    #[test]
    fn test_merge_prev() {
        let mut heap = init_heap_fragmented();
        heap.free(1024); // the block should be merged with the previous one at addr 512
        assert_eq!(heap.blocks.len(), 3);
        assert_eq!(heap.blocks[&512].available, true);
        assert_eq!(heap.blocks[&512].size, 1024);
        assert_eq!(heap.blocks[&512].prev, Some(0));
        assert_eq!(heap.blocks[&512].next, Some(1536));
    }

    #[test]
    fn test_merge_next() {
        let mut heap = init_heap_fragmented();
        heap.free(0); // the block should be merged with the next one at addr 512
        assert_eq!(heap.blocks.len(), 3);
        assert_eq!(heap.blocks[&0].available, true);
        assert_eq!(heap.blocks[&0].size, 1024);
        assert_eq!(heap.blocks[&0].prev, None);
        assert_eq!(heap.blocks[&0].next, Some(1024));
        assert_eq!(heap.blocks[&1024].available, false);
    }

    #[test]
    fn test_out_of_memory() {
        let mut heap = Heap::new(0, 512);
        assert_eq!(heap.malloc(1024), None);
    }
}