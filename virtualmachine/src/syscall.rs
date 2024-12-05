use common::{OBJECT_HEADER_SIZE, SYSCALL_ABORT, SYSCALL_ARRAY_ALLOC, SYSCALL_ARRAY_GET, SYSCALL_ARRAY_PUT, SYSCALL_BREAKPOINT, SYSCALL_CANVAS_INIT, SYSCALL_CANVAS_UPDATE, SYSCALL_IO_OUT_FLOAT, SYSCALL_IO_OUT_INT, SYSCALL_IO_OUT_NL, SYSCALL_IO_OUT_STRING, SYSCALL_STRING_EQUALS, SYSCALL_STRING_LEN, SYSCALL_SYSTEM_EXIT, SYSCALL_SYSTEM_MALLOC};
use crate::memory::PhysicalMemory;
use crate::process::Process;
use crate::utils::Interruption;
use log::error;

pub fn execute_syscall(sb: u32, sp: u32, process: &mut Process, memory: &mut PhysicalMemory, syscall_num: u8) -> Result<u32, Interruption> {
    let wrapped_first_arg_addr = sb + 4; // when syscall is wrapped into a method, use SB+4 as first arg (+4 to skip the "this" pointer)
    let inline_first_arg_addr = sp; // when syscall is inlined, the args are on SP (the syscall is not wrapped in a method call)
    match syscall_num {
        //
        // System syscalls
        //
        SYSCALL_SYSTEM_EXIT => { // inlined syscall, no args
            Err(Interruption::ProcessTermination)
        }
        SYSCALL_SYSTEM_MALLOC => { // inlined syscall, single arg
            let requested_size = memory.read_u32(process, inline_first_arg_addr);
            let allocated_addr = process.heap.malloc(requested_size);
            match allocated_addr {
                Some(addr) => Ok(addr),
                None => {
                    error!("Cannot allocate {requested_size} bytes on heap");
                    Err(Interruption::ProcessFault)
                }
            }
        }

        //
        // IO syscalls
        //
        SYSCALL_IO_OUT_INT => { // wrapped syscall, single arg
            let i = memory.read_u32(process, wrapped_first_arg_addr) as i32;
            write!(process.stdout, "{i}").map_err(|_| {
                error!("Cannot print int");
                Interruption::ProcessFault
            })?;
            Ok(4) // number of bytes written
        }
        SYSCALL_IO_OUT_FLOAT => { // wrapped syscall, 2 args
            let f = f32::from_bits(memory.read_u32(process, wrapped_first_arg_addr));
            let digits = memory.read_u32(process, wrapped_first_arg_addr + 4) as usize;
            write!(process.stdout, "{:.1$}", f, digits).map_err(|_| {
                error!("Cannot print float");
                Interruption::ProcessFault
            })?;
            Ok(4) // number of bytes written
        }
        SYSCALL_IO_OUT_STRING => { // wrapped syscall, single arg
            let addr = memory.read_u32(process, wrapped_first_arg_addr);
            let string = memory.read_string(process, addr);
            write!(process.stdout, "{}", string).map_err(|_| {
                error!("Cannot print string");
                Interruption::ProcessFault
            })?;
            Ok(string.len() as u32) // number of bytes written
        }
        SYSCALL_IO_OUT_NL => { // wrapped syscall, no args
            write!(process.stdout, "\n").map_err(|_| {
                error!("Cannot print newline");
                Interruption::ProcessFault
            })?;
            Ok(1) // number of bytes written
        }
        SYSCALL_ABORT => {
            let addr = memory.read_u32(process, wrapped_first_arg_addr);
            let message = memory.read_string(process, addr);
            write!(process.stdout, "{}", message).map_err(|_| {
                error!("Cannot print abort message");
                Interruption::ProcessFault
            })?;
            Err(Interruption::ProcessTermination)
        }
        SYSCALL_BREAKPOINT => {
            process.debugger.set_breakpoint();
            Ok(0)
        }

        //
        // Array syscalls
        //
        SYSCALL_ARRAY_ALLOC => {  // wrapped syscall, single arg
            let array_length = memory.read_u32(process, wrapped_first_arg_addr);
            let allocated_addr = process.heap.malloc(array_length * 4);
            match allocated_addr {
                Some(addr) => {
                    memory.zerofill_u32(process, addr, array_length);
                    Ok(addr)
                },
                None => {
                    error!("Cannot allocate array of size {array_length} on heap");
                    Err(Interruption::ProcessFault)
                } 
            }
        }
        x if x == SYSCALL_ARRAY_GET || x == SYSCALL_ARRAY_PUT  => {  // wrapped syscall with this
            let index = memory.read_u32(process, wrapped_first_arg_addr);
            let this_addr = memory.read_u32(process, sb);
            let array_len = memory.read_u32(process, this_addr + OBJECT_HEADER_SIZE as u32);

            if index < 0 || index > array_len {
                error!("Array access out of bounds: {index}");
                return Err(Interruption::ProcessFault);
            }
            let array_buf_addr = memory.read_u32(process, this_addr + OBJECT_HEADER_SIZE as u32 + 4);
            let element_addr = array_buf_addr + index * 4;

            if x == SYSCALL_ARRAY_GET {
                let value = memory.read_u32(process, element_addr);
                Ok(value)
            } else {
                let value = memory.read_u32(process, wrapped_first_arg_addr + 4);
                memory.write_u32(process, element_addr, value);
                Ok(value)
            }
        }

        //
        // String syscalls
        //
        SYSCALL_STRING_LEN => {  // wrapped syscall, no arg
            let addr = memory.read_u32(process, sb);
            let string = memory.read_string(process, addr);
            Ok(string.len() as u32)
        }
        SYSCALL_STRING_EQUALS => { // wrapped syscall, single arg
            let addr_a = memory.read_u32(process, sb);
            let string_a = memory.read_string(process, addr_a);
            let addr_b = memory.read_u32(process, wrapped_first_arg_addr);
            let string_b = memory.read_string(process, addr_b);
            let cmp = string_a == string_b;
            Ok(u32::from(cmp))
        }

        //
        // Canvas syscalls
        //
        SYSCALL_CANVAS_INIT => { // wrapped syscall, 2 args
            let w = memory.read_u32(process, wrapped_first_arg_addr);
            let h = memory.read_u32(process, wrapped_first_arg_addr + 4);
            Err(Interruption::GraphicsInit(w, h))
        }
        SYSCALL_CANVAS_UPDATE => { // wrapped syscall, 1 arg
            let array_buffer_addr = memory.read_u32(process, wrapped_first_arg_addr + OBJECT_HEADER_SIZE as u32 + 4); // skip array length field
            Err(Interruption::GraphicsUpdate(array_buffer_addr))
        }

        _ => {
            error!("Unsupported syscall {syscall_num}");
            Err(Interruption::KernelPanic)
        }
    }
}
