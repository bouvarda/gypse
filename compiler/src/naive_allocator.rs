use crate::bytecode::OpCode::{Mov, Pop, Push};
use crate::bytecode::Operand::{
    MemoryAtRegister, MemoryAtRegisterOffset, MemoryAtRegisterOffsetScale, RegisterValue,
};
use crate::bytecode::Register::{GP, SB};
use crate::bytecode::{Bytecode, Label, Operand, Register};
use crate::naive_allocator::AllocatedResult::{PhysicalRegister, SpillLocation};
use crate::transformer::{bytecode_transform, BytecodeTransformer};
use std::collections::HashMap;

#[derive(PartialOrd, PartialEq, Clone)]
enum AllocatedResult {
    PhysicalRegister(usize),
    SpillLocation(i16),
}

pub struct NaiveAllocator {
    max_phys_registers: usize,
    next_free_register: usize,
    next_free_spill_location: i16,
    allocated_map: HashMap<usize, AllocatedResult>,
}

impl BytecodeTransformer for NaiveAllocator {
    fn transform_read(&mut self, op: Operand, bytecode: &mut Bytecode) -> Operand {
        match op {
            RegisterValue(Register::Virtual(i)) => RegisterValue(self.alloc_read(i, bytecode)),
            MemoryAtRegister(Register::Virtual(i)) => {
                MemoryAtRegister(self.alloc_read(i, bytecode))
            }
            MemoryAtRegisterOffset(Register::Virtual(i), o) => {
                MemoryAtRegisterOffset(self.alloc_read(i, bytecode), o)
            }
            MemoryAtRegisterOffsetScale(Register::Virtual(i), o, j, s) => {
                MemoryAtRegisterOffsetScale(self.alloc_read(i, bytecode), o, j, s)
            }
            _ => op,
        }
    }

    fn transform_write(&mut self, op: Operand, _bytecode: &mut Bytecode) -> Operand {
        match op {
            RegisterValue(Register::Virtual(virtual_reg)) => match self.alloc_write(virtual_reg) {
                PhysicalRegister(phys_reg) => RegisterValue(GP(phys_reg)),
                SpillLocation(spill_loc) => MemoryAtRegisterOffset(SB, spill_loc as i16),
            },
            MemoryAtRegister(Register::Virtual(virtual_reg)) => {
                match self.alloc_write(virtual_reg) {
                    PhysicalRegister(phys_reg) => MemoryAtRegister(GP(phys_reg)),
                    SpillLocation(spill_loc) => MemoryAtRegisterOffset(SB, spill_loc as i16),
                }
            }
            MemoryAtRegisterOffset(Register::Virtual(virtual_reg), offset) => {
                match self.alloc_write(virtual_reg) {
                    PhysicalRegister(phys_reg) => MemoryAtRegisterOffset(GP(phys_reg), offset),
                    SpillLocation(_) => panic!("Unsupported write access to {virtual_reg}"),
                }
            }
            MemoryAtRegisterOffsetScale(Register::Virtual(virtual_reg), offset, j, s) => {
                match self.alloc_write(virtual_reg) {
                    PhysicalRegister(phys_reg) => {
                        MemoryAtRegisterOffsetScale(GP(phys_reg), offset, j, s)
                    }
                    SpillLocation(_) => panic!("Unsupported write access to {virtual_reg}"),
                }
            }
            _ => op,
        }
    }

    fn transform_pre_call(&mut self, _bytecode: &mut Bytecode) {
        // caller-based backup
        /*for r in self.allocated_regs(true) {
            bytecode.emit(Push(L, RegisterValue(GP(r))), Some("save register before call"));
        }*/
    }

    fn transform_post_call(&mut self, _bytecode: &mut Bytecode) {
        // caller-based restore
        /*for r in self.allocated_regs(false) {
            bytecode.emit(Pop(L, RegisterValue(GP(r))), Some("restore register after call"));
        }*/
    }

    fn transform_return(&mut self, label: Option<Label>, bytecode: &mut Bytecode) {
        // move the first label if any, as we will emit at the beginning
        let mut saved_label: Option<String> = None;
        if bytecode.text.len() > 0
            && bytecode.text[0].label.is_some()
            && self.allocated_map.len() > 0
        {
            saved_label = bytecode.text[0].label.take();
        }
        // callee-based backup
        for r in self.allocated_regs(false) {
            bytecode.emit_first(
                Push(RegisterValue(GP(r))),
                Some("save callee-based register"),
            );
        }
        if saved_label.is_some() {
            bytecode.text[0].label = saved_label;
        }
        // the "ret" label must be put before the restore of callee-based registers
        if label.is_some() {
            bytecode.set_label_next(label.unwrap());
        }

        // callee-based restore
        for r in self.allocated_regs(false) {
            bytecode.emit(
                Pop(RegisterValue(GP(r))),
                Some("restore callee-based register"),
            );
        }
    }
}

impl NaiveAllocator {
    pub fn new(max_phys_registers: usize, next_free_spill_location: i16) -> NaiveAllocator {
        NaiveAllocator {
            max_phys_registers,
            next_free_register: 0,
            next_free_spill_location,
            allocated_map: HashMap::new(),
        }
    }

    fn swap_phys_register(&self) -> Register {
        GP(self.max_phys_registers - 1)
    }

    fn alloc_write(&mut self, virtual_reg: usize) -> AllocatedResult {
        if self.allocated_map.contains_key(&virtual_reg) {
            self.allocated_map[&virtual_reg].clone()
        } else if self.next_free_register < self.max_phys_registers - 1 {
            let phys_reg = PhysicalRegister(self.next_free_register);
            self.next_free_register += 1;
            self.allocated_map.insert(virtual_reg, phys_reg.clone());
            phys_reg
        } else {
            let spill_location = SpillLocation(self.next_free_spill_location);
            self.next_free_spill_location -= 4;
            self.allocated_map
                .insert(virtual_reg, spill_location.clone());
            spill_location
        }
    }

    fn spill_candidate(&self) -> (usize, usize) {
        let mut result: Option<(usize, usize)> = None;
        let mut smaller: Option<usize> = None;
        for (virtual_reg, value) in &self.allocated_map {
            if let PhysicalRegister(phys_reg) = value {
                if smaller.is_none() || phys_reg < &smaller.unwrap() {
                    result = Some((*virtual_reg, *phys_reg));
                    smaller = Some(*phys_reg);
                }
            }
        }
        result.expect("Cannot find spill candidate")
    }

    fn alloc_read(&mut self, virtual_reg: usize, bytecode: &mut Bytecode) -> Register {
        let mut spill: Option<(usize, i16)> = None;
        let result = match self.allocated_map.get(&virtual_reg) {
            Some(PhysicalRegister(phys_reg)) => GP(*phys_reg),
            Some(SpillLocation(spill_loc)) => {
                let (spill_virtual_reg, spill_phys_reg) = self.spill_candidate();
                spill = Some((spill_virtual_reg, *spill_loc));
                GP(spill_phys_reg)
            }
            None => panic!("Cannot allocate read operand"),
        };
        if let Some((spill_virtual_reg, spill_loc)) = spill {
            let PhysicalRegister(reg_id) = self.allocated_map.remove(&spill_virtual_reg).unwrap()
            else {
                panic!("Not physical")
            };
            let swap: Register = self.swap_phys_register();
            bytecode.emit(
                Mov(RegisterValue(swap.clone()), RegisterValue(GP(reg_id))),
                Some("save register before spill"),
            );
            bytecode.emit(
                Mov(
                    RegisterValue(GP(reg_id)),
                    MemoryAtRegisterOffset(SB, spill_loc as i16),
                ),
                Some("load spilled register"),
            );
            bytecode.emit(
                Mov(
                    MemoryAtRegisterOffset(SB, spill_loc as i16),
                    RegisterValue(swap),
                ),
                Some("spill previous register"),
            );
            self.allocated_map
                .insert(spill_virtual_reg, SpillLocation(spill_loc));
            self.allocated_map
                .insert(virtual_reg, PhysicalRegister(reg_id));
        }
        result
    }

    fn allocated_regs(&self, asc_order: bool) -> Vec<usize> {
        let mut regs: Vec<usize> = vec![];
        for result in self.allocated_map.values() {
            match result {
                PhysicalRegister(phys_reg) => regs.push(*phys_reg),
                SpillLocation(_) => {}
            }
        }
        regs.sort();
        if !asc_order {
            regs.reverse()
        }
        regs
    }

    pub fn allocate(&mut self, bytecode: Bytecode) -> Bytecode {
        bytecode_transform(bytecode, self)
    }
}

#[cfg(test)]
mod tests {
    use crate::bytecode::OpCode::{Add, Call, Cmp, Jmp, Jne, Mov, Ret};
    use crate::bytecode::Operand::{ImmediateValue, MemoryAtRegister};
    use crate::bytecode::Register::Virtual;
    use crate::bytecode::{Bytecode, Label};
    use crate::naive_allocator::NaiveAllocator;

    fn gen_if_else_bytecode() -> Bytecode {
        let mut bytecode = Bytecode::new();
        bytecode.set_label_next(Label::from("if-start"));
        bytecode.emit(Mov(MemoryAtRegister(Virtual(1)), ImmediateValue(1)), None);
        bytecode.emit(Mov(MemoryAtRegister(Virtual(2)), ImmediateValue(2)), None);
        bytecode.emit(
            Cmp(MemoryAtRegister(Virtual(1)), MemoryAtRegister(Virtual(2))),
            None,
        );
        bytecode.emit(Jne(Label::from("if-then")), None);
        bytecode.emit(
            Mov(MemoryAtRegister(Virtual(3)), MemoryAtRegister(Virtual(2))),
            None,
        );
        bytecode.emit(Call(Label::from("else")), None);
        bytecode.emit(Jmp(Label::from("if-end")), None);
        bytecode.set_label_next(Label::from("if-then"));
        bytecode.emit(
            Mov(MemoryAtRegister(Virtual(3)), MemoryAtRegister(Virtual(1))),
            None,
        );
        bytecode.emit(Call(Label::from("then")), None);
        bytecode.set_label_next(Label::from("if-end"));
        bytecode.emit(
            Mov(MemoryAtRegister(Virtual(4)), MemoryAtRegister(Virtual(3))),
            None,
        );
        bytecode.emit(Add(MemoryAtRegister(Virtual(4)), ImmediateValue(10)), None);
        bytecode.emit(Ret, None);
        bytecode
    }

    #[test]
    fn test_allocation_if_else_enough_registers() {
        let bytecode = gen_if_else_bytecode();
        let mut allocator = NaiveAllocator::new(5, 0);
        let allocated_bytecode = allocator.allocate(bytecode);
        let asm = allocated_bytecode.print_text_assembly("test");
        let asm_trimmed: Vec<_> = asm.split("\n").map(|l| l.trim_end()).collect();
        assert_eq!(
            asm_trimmed.join("\n"),
            "\
.entry test
.data
.text
  if-start:
      push    r0                  ; save callee-based register
      push    r1                  ; save callee-based register
      push    r2                  ; save callee-based register
      push    r3                  ; save callee-based register
      mov     [r0], 1
      mov     [r1], 2
      cmp     [r0], [r1]
      jne     if-then
      mov     [r2], [r1]
      call    else
      jmp     if-end
  if-then:
      mov     [r2], [r0]
      call    then
  if-end:
      mov     [r3], [r2]
      add     [r3], 10
      pop     r3                  ; restore callee-based register
      pop     r2                  ; restore callee-based register
      pop     r1                  ; restore callee-based register
      pop     r0                  ; restore callee-based register
      ret\n"
        );
    }

    #[test]
    fn test_allocation_if_else_not_enough_registers() {
        let bytecode = gen_if_else_bytecode();
        let mut allocator = NaiveAllocator::new(4, 0);
        let allocated_bytecode = allocator.allocate(bytecode);
        let asm = allocated_bytecode.print_text_assembly("test");
        let asm_trimmed: Vec<_> = asm.split("\n").map(|l| l.trim_end()).collect();
        assert_eq!(
            asm_trimmed.join("\n"),
            "\
.entry test
.data
.text
  if-start:
      push    r0                  ; save callee-based register
      push    r1                  ; save callee-based register
      push    r2                  ; save callee-based register
      mov     [r0], 1
      mov     [r1], 2
      cmp     [r0], [r1]
      jne     if-then
      mov     [r2], [r1]
      call    else
      jmp     if-end
  if-then:
      mov     [r2], [r0]
      call    then
  if-end:
      mov     [sb - 0], [r2]
      mov     r3, r0              ; save register before spill
      mov     r0, [sb - 0]        ; load spilled register
      mov     [sb - 0], r3        ; spill previous register
      add     [r0], 10
      pop     r2                  ; restore callee-based register
      pop     r1                  ; restore callee-based register
      pop     r0                  ; restore callee-based register
      ret\n"
        );
    }
}
