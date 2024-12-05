use std::io::{stdin, stdout, Write};
use std::str::FromStr;
use crate::debugger::DebuggerAction::{LetGo, PrintHeap, PrintMemoryAt, PrintRegisters, PrintStack, PrintData};
use crate::FLAG_ENABLE_DEBUGGER;

pub enum DebuggerAction {
    LetGo,
    PrintRegisters,
    PrintStack,
    PrintHeap,
    PrintData,
    PrintMemoryAt(u32)
}

pub struct Debugger {
    ticks_before_pause: isize,
    breakpoint_enabled: bool,
}

impl Debugger {
    pub fn new(vm_flags: u8) -> Debugger {
        if vm_flags & FLAG_ENABLE_DEBUGGER == 1 {
            Debugger { ticks_before_pause: 0, breakpoint_enabled: false }
        } else {
            Debugger { ticks_before_pause: -1, breakpoint_enabled: false }
        }
    }

    pub fn tick(&mut self) -> DebuggerAction {
        if self.ticks_before_pause == 0 || self.breakpoint_enabled {
            self.breakpoint_enabled = false;
            self.ticks_before_pause = 0;
            self.interactive_loop()
        } else if self.ticks_before_pause > 0 {
            self.ticks_before_pause -= 1;
            LetGo
        } else { // disabled
            LetGo
        }
    }

    pub fn set_breakpoint(&mut self) {
        self.breakpoint_enabled = true;
    }

    fn interactive_loop(&mut self) -> DebuggerAction {
        loop {
            println!("Debugger paused, enter command:");
            print!(">");
            stdout().flush().expect("Cannot flush");
            let mut command: String = String::new();
            stdin().read_line(&mut command).expect("Please enter a command");

            match command.trim() {
                "help" | "h" => self.command_help(),
                "exit" => panic!("Exiting program"),
                "resume" => {
                    self.ticks_before_pause = -1;
                    return LetGo
                },
                "next" | "n" => {
                    self.ticks_before_pause = 0;
                    return LetGo
                }
                "reg" => {
                    return PrintRegisters
                }
                "stack" | "s" => {
                    return PrintStack
                }
                "heap" => {
                    return PrintHeap
                }
                "data" => {
                    return PrintData
                }
                c if c.starts_with("mem ") => {
                    let s = c.split_whitespace().nth(1).expect("Expected two args: mem <addr>");
                    let addr = u32::from_str(s).expect("Expected 'mem <addr>'");
                    return PrintMemoryAt(addr)
                }
                c if c.starts_with("step ") => {
                    let s = c.split_whitespace().nth(1).expect("Expected two args: step <number>");
                    let n = usize::from_str(s).expect("Expected 'step <number>'");
                    self.ticks_before_pause = n as isize - 1;
                    return LetGo
                }
                _ => {
                    println!("Unknown command");
                    self.command_help()
                }
            }
        }
    }

    fn command_help(&self) {
        println!("Commands: next, step <N>, reg, stack, heap, data, mem <N>, resume, exit");
    }
}