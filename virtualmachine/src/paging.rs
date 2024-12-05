use std::collections::HashMap;

// Virtual address format
// [31        21             11           0]
//  dir_offset | table_offset | page offset

#[derive(PartialEq, Debug)]
pub struct PageFault;

#[derive(PartialEq, Debug, Clone)]
struct Page {
    physical_page_frame: u32,
    // flags
    // present, read/write, execute
}

#[derive(PartialEq, Debug, Clone)]
struct PageTable {
    pages: HashMap<u32, Page>,
}

impl PageTable {
    pub fn create_page(&mut self, page_offset: u32, physical_page_frame: u32) -> &Page {
        if !self.pages.contains_key(&page_offset) {
            self.pages.insert(page_offset, Page { physical_page_frame});
        }
        self.pages.get(&page_offset).unwrap()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Paging {
    page_directory: HashMap<u32, PageTable>,
}

impl Paging {
    pub fn new() -> Paging {
        Paging {
            page_directory: HashMap::new(),
        }
    }

    fn get_page_directory_offset(&self, virtual_addr: u32) -> u32 {
        virtual_addr >> 22 // get 10 bits from index 22 to 31
    }

    fn get_page_table_offset(&self, virtual_addr: u32) -> u32 {
        (virtual_addr >> 12) & 0x03FF // get 10 bits from index 12 to 21
    }

    fn get_page_directory(&self, virtual_addr: u32) -> Result<&PageTable, PageFault> {
        let page_directory_offset = self.get_page_directory_offset(virtual_addr);
        if self.page_directory.contains_key(&page_directory_offset) {
            Ok(self.page_directory.get(&page_directory_offset).unwrap())
        } else {
            Err(PageFault)
        }
    }

    fn create_page_table(&mut self, virtual_addr: u32) -> &mut PageTable {
        let page_directory_offset = self.get_page_directory_offset(virtual_addr);
        if !self.page_directory.contains_key(&page_directory_offset) {
            self.page_directory.insert(page_directory_offset, PageTable { pages: HashMap::new() });
        }
        self.page_directory.get_mut(&page_directory_offset).unwrap()
    }

    fn get_page(&self, virtual_addr: u32) -> Result<&Page, PageFault> {
        let page_directory = self.get_page_directory(virtual_addr)?;
        let page_table_offset = self.get_page_table_offset(virtual_addr);
        if page_directory.pages.contains_key(&page_table_offset) {
            Ok(page_directory.pages.get(&page_table_offset).unwrap())
        } else {
            Err(PageFault)
        }
    }

    pub fn translate_virtual_to_physical(&self, virtual_addr: u32) -> Result<u32, PageFault> {
        let page = self.get_page(virtual_addr)?;
        let page_offset = virtual_addr & 0x0FFF; // get 12 bits from index 0 to 11
        Ok(page.physical_page_frame + page_offset)
    }

    pub fn assign_page_frame_mapping(&mut self, virtual_addr: u32, physical_page_frame: u32) {
        let page_table_offset = self.get_page_table_offset(virtual_addr);
        let page_table: &mut PageTable = self.create_page_table(virtual_addr);
        page_table.create_page(page_table_offset, physical_page_frame);
    }
}

#[cfg(test)]
mod tests {
    use crate::paging::{PageFault, Paging};

    #[test]
    fn test_virtual_memory_translation() {
        let mut mem = Paging::new();
        assert_eq!(mem.translate_virtual_to_physical(0xA0000000), Err(PageFault));
        mem.assign_page_frame_mapping(0xA0000000, 0xB0000000);
        assert_eq!(mem.translate_virtual_to_physical(0xA0000000), Ok(0xB0000000)); // same page same addr
        assert_eq!(mem.translate_virtual_to_physical(0xA0000111), Ok(0xB0000111)); // same page different addr
        assert_eq!(mem.translate_virtual_to_physical(0xA0100111), Err(PageFault)); // another page

        assert_eq!(mem.translate_virtual_to_physical(0x00000001), Err(PageFault));
        mem.assign_page_frame_mapping(0x00000100, 0xFFF0000);
        assert_eq!(mem.translate_virtual_to_physical(0x00000001), Ok(0xFFF0001));
    }
}