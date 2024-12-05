//!
//! Compilation scope type definitions and helpers
//!
use crate::ast::Class;
use crate::bytecode::Register;
use crate::bytecode::Register::Virtual;
use crate::bytecode::{Label, Operand};
use std::collections::hash_map::Entry;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub struct ScopeVar {
    pub var_type: String,
    pub location: Operand,
}

#[derive(PartialEq, Debug, Clone)]
struct ScopeFrame {
    vars: HashMap<String, ScopeVar>,
    next_stack_addr: i16,
    allocated_size: i16,
    start_spill_addr: i16,
    virtual_register_counter: usize,
    virtual_register_map: HashMap<Operand, usize>,
}

impl ScopeFrame {
    fn declare_var(&mut self, var_name: &str, var_type: &str, location: Operand) {
        let entry = self.vars.entry(String::from(var_name));
        match entry {
            Entry::Occupied(_) => panic!("Variable already exist in the scope"),
            Entry::Vacant(_) => {
                entry.or_insert(ScopeVar {
                    var_type: String::from(var_type),
                    location,
                });
            }
        }
    }

    pub fn alloc_stack(&mut self, size: i16) -> i16 {
        let result = self.next_stack_addr;
        self.next_stack_addr -= size;
        self.allocated_size += size;
        self.start_spill_addr -= size;
        result
    }

    pub fn allocated_size(&self) -> i16 {
        self.allocated_size
    }

    pub fn alloc_virtual_register(&mut self) -> Register {
        let id = self.virtual_register_counter;
        self.virtual_register_counter += 1;
        Virtual(id)
    }

    fn alloc_virtual_register_from(&mut self, target: &Operand) -> Register {
        if self.virtual_register_map.contains_key(target) {
            Virtual(self.virtual_register_map[target])
        } else {
            self.virtual_register_counter += 1;
            self.virtual_register_map
                .insert(target.clone(), self.virtual_register_counter);
            Virtual(self.virtual_register_counter)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scope {
    class: Option<String>,
    frames: Vec<ScopeFrame>,
    label_counter: usize,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            class: None,
            frames: vec![],
            label_counter: 0,
        }
    }

    pub fn current_class(&self) -> String {
        self.class.clone().unwrap()
    }

    pub fn set_class(&mut self, class: &str) {
        self.class = Some(String::from(class));
    }

    pub fn push_scope(&mut self) {
        let new_frame = match self.current_frame() {
            Some(parent_frame) => {
                // inherit from parent values
                ScopeFrame {
                    vars: HashMap::new(),
                    next_stack_addr: parent_frame.next_stack_addr,
                    allocated_size: 0,
                    start_spill_addr: parent_frame.next_stack_addr,
                    virtual_register_counter: parent_frame.virtual_register_counter,
                    virtual_register_map: parent_frame.virtual_register_map.clone(),
                }
            }
            None => {
                // root frame
                let next_stack_addr = -8; // the 4 first bytes are taken by the IP return ptr
                ScopeFrame {
                    vars: HashMap::new(),
                    next_stack_addr,
                    allocated_size: 0,
                    start_spill_addr: next_stack_addr,
                    virtual_register_counter: 1,
                    virtual_register_map: HashMap::new(),
                }
            }
        };
        self.frames.push(new_frame);
    }

    pub fn declare_var(&mut self, var_name: &str, var_type: &str, location: Operand) {
        self.current_frame()
            .map(|f| f.declare_var(var_name, var_type, location));
    }

    pub fn alloc_virtual_register(&mut self) -> Register {
        let frame = self.current_frame().expect("Cannot get current frame");
        frame.alloc_virtual_register()
    }

    pub fn alloc_virtual_register_from(&mut self, target: &Operand) -> Register {
        let frame = self.current_frame().expect("Cannot get current frame");
        frame.alloc_virtual_register_from(target)
    }

    pub fn alloc_stack(&mut self, size: i16) -> i16 {
        self.current_frame().map(|f| f.alloc_stack(size)).unwrap()
    }

    pub fn allocated_size(&mut self) -> i16 {
        self.current_frame().map(|f| f.allocated_size()).unwrap()
    }

    pub fn start_spill_addr(&mut self) -> i16 {
        self.current_frame().map(|f| f.start_spill_addr).unwrap()
    }

    pub fn gen_label(&mut self, name: &str) -> Label {
        let mut label = self.current_class();
        label.push_str("::_");
        label.push_str(name);
        label.push_str("_");
        label.push_str(self.label_counter.to_string().as_str());
        self.label_counter += 1;
        label
    }

    pub fn gen_class_label(class_type: &str, name: &str) -> Label {
        let mut label = String::new();
        label.push_str(class_type);
        label.push_str("::");
        label.push_str(name);
        label
    }

    pub fn pop_scope(&mut self, preserve_virtual_counter: bool) {
        let mut counter: Option<usize> = None;
        let mut start_spill_addr: Option<i16> = None;
        if preserve_virtual_counter && !self.frames.is_empty() {
            let frame = self.current_frame().unwrap();
            counter = Some(frame.virtual_register_counter);
            start_spill_addr = Some(frame.start_spill_addr);
        }
        self.frames.pop().expect("Cannot pop empty scope");
        if counter.is_some() && !self.frames.is_empty() {
            let frame = self.current_frame().unwrap();
            frame.virtual_register_counter = counter.unwrap();
            frame.start_spill_addr = start_spill_addr.unwrap();
        }
    }

    pub fn lookup(&self, var_name: &str) -> Option<&ScopeVar> {
        for s in self.frames.iter().rev() {
            if s.vars.contains_key(var_name) {
                return s.vars.get(var_name);
            }
        }
        None
    }

    pub fn lookup_layout_index(&self, class: &Class, attribute_name: &str) -> Option<i16> {
        class.find_field_index(attribute_name)
    }

    fn current_frame(&mut self) -> Option<&mut ScopeFrame> {
        if self.frames.is_empty() {
            return None;
        }
        let idx = self.frames.len() - 1;
        self.frames.get_mut(idx)
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode::Operand::{MemoryAtRegisterOffset, RegisterValue};
    use crate::bytecode::Register;
    use crate::bytecode::Register::GP;
    use crate::scope::{Scope, ScopeVar};

    #[test]
    fn test_scope_vars() {
        let mut scope = Scope::new();
        scope.set_class("A");
        scope.push_scope();
        scope.declare_var("a", "Int", MemoryAtRegisterOffset(GP(1), 4));
        scope.declare_var("b", "Array", MemoryAtRegisterOffset(GP(2), 8));
        scope.push_scope();
        scope.declare_var("a", "Long", MemoryAtRegisterOffset(GP(1), 8));
        scope.declare_var("b2", "Bool", MemoryAtRegisterOffset(GP(2), 1));
        scope.push_scope();
        scope.declare_var("b3", "A", RegisterValue(GP(1)));

        assert_eq!(
            scope.lookup("a"),
            Some(&ScopeVar {
                var_type: String::from("Long"),
                location: MemoryAtRegisterOffset(GP(1), 8)
            })
        );
        assert_eq!(
            scope.lookup("b"),
            Some(&ScopeVar {
                var_type: String::from("Array"),
                location: MemoryAtRegisterOffset(GP(2), 8)
            })
        );
        assert_eq!(
            scope.lookup("b2"),
            Some(&ScopeVar {
                var_type: String::from("Bool"),
                location: MemoryAtRegisterOffset(GP(2), 1)
            })
        );
        assert_eq!(
            scope.lookup("b3"),
            Some(&ScopeVar {
                var_type: String::from("A"),
                location: RegisterValue(GP(1))
            })
        );
    }

    #[test]
    fn test_scope_register_allocation() {
        let mut scope = Scope::new();
        scope.set_class("A");
        scope.push_scope();
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(1));
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(2));
        scope.push_scope();
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(3));
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(4));
        scope.pop_scope(false);
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(3));
        scope.push_scope();
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(4));
        scope.pop_scope(false);
        assert_eq!(scope.alloc_virtual_register(), Register::Virtual(4));
    }

    #[test]
    fn test_scope_gen_label() {
        let mut scope = Scope::new();
        scope.set_class("A");
        assert_eq!(scope.gen_label("block"), "A::_block_0");
        assert_eq!(scope.gen_label("block"), "A::_block_1");
        assert_eq!(scope.gen_label("if_else"), "A::_if_else_2");
    }
}
