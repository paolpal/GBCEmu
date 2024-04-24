use crate::memory::wram::WRam;
use std::fmt;

pub struct Registers {
	pub a: u8,
	pub f: u8,
	pub b: u8,
	pub c: u8,
	pub d: u8,
	pub e: u8,
	pub h: u8,
	pub l: u8,
	pub sp: u16,
	pub pc: u16,
}

pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
    pub ime: bool,
}

impl fmt::Display for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Registers:
             A: {:02X}
             F: {:02X}
             B: {:02X}
             C: {:02X}
             D: {:02X}
             E: {:02X}
             H: {:02X}
             L: {:02X}
             SP: {:04X}
             PC: {:04X}",
            self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc
        )
    }
}

pub struct Processor {
    pub regs : Registers,
    pub fs : Flags,
}

impl Processor {
    pub fn new() -> Processor {
        Processor {
            regs: Registers {
                a: 0,
                f: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                h: 0,
                l: 0,
                sp: 0,
                pc: 0,
            }, 
            fs: Flags {
                z: false,
                n: false,
                h: false,
                c: false,
                ime: false,
            }
        }
    }

    fn push(&mut self, memory: &mut WRam, value: u16) {
        self.regs.sp -= 1;
        memory.write(self.regs.sp, (value >> 8) as u8);
        self.regs.sp -= 1;
        memory.write(self.regs.sp, value as u8);
    }

    fn pop(&mut self, memory: &mut WRam) -> u16 {
        let low = memory.read(self.regs.sp);
        self.regs.sp += 1;
        let high = memory.read(self.regs.sp);
        self.regs.sp += 1;
        (high as u16) << 8 | low as u16
    }

    pub fn print_state(&self) {
        println!("{}", self.regs);
    }

    fn next_byte(&mut self, memory: &WRam) -> u8 {
        let byte = memory.read(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        byte
    }

    fn next_word(&mut self, memory: &WRam) -> u16 {
        let word = (self.next_byte(memory) as u16) << 8 | self.next_byte(memory) as u16;
        word
    }

    fn bit(&mut self, reg: u8, bit: u8) {
        self.fs.z = (reg & (1 << bit)) == 0;
        self.fs.n = false;
        self.fs.h = true;
    }

    fn set(&mut self, reg: &mut u8, bit: u8) {
        *reg |= 1 << bit;
    }

    fn res(&mut self, reg: &mut u8, bit: u8) {
        *reg &= !(1 << bit);
    }

    pub fn step(&mut self, memory: &mut WRam) -> usize {
        let opcode = self.next_byte(memory);
        match opcode {
            0x00 => { 4 }, // NOP
            0x01 => { 
                self.regs.c = self.next_byte(memory);
                self.regs.b = self.next_byte(memory);
                12 
            }, // LD BC,u16
            0x11 => { 
                self.regs.e = self.next_byte(memory);
                self.regs.d = self.next_byte(memory);
                12 
            }, // LD DE,u16
            0x21 => { 
                self.regs.l = self.next_byte(memory);
                self.regs.h = self.next_byte(memory);
                12 
            }, // LD HL,u16
            0x31 => { 
                let p = self.next_byte(memory);
                let s = self.next_byte(memory);
                self.regs.sp = (s as u16) << 8 | p as u16;
                12 
            }, // LD SP,u16
            0x02 => { 
                let addr: u16 = (self.regs.b as u16) << 8 | self.regs.c as u16;
                memory.write(addr, self.regs.a);
                8 
            }, // LD (BC),A
            0x12 => { 
                let addr: u16 = (self.regs.d as u16) << 8 | self.regs.e as u16;
                memory.write(addr, self.regs.a);
                8 
            }, // LD (DE),A
            0x22 => { 
                let mut addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                addr+=1;
                memory.write(addr, self.regs.a);
                8 
            }, // LD (HL+),A
            0x32 => { 
                let mut addr: u16 = (self.regs.b as u16) << 8 | self.regs.c as u16;
                addr-=1;
                memory.write(addr, self.regs.a);
                8 
            }, // LD (HL-),A
            0x03 => {
                let combined: u16 = (self.regs.b as u16) << 8 | self.regs.c as u16;
                let incremented: u16 = combined + 1;
                self.regs.b = (incremented >> 8) as u8;
                self.regs.c = incremented as u8;
                8
            }, // INC BC
            0x13 => {
                let combined: u16 = (self.regs.d as u16) << 8 | self.regs.e as u16;
                let incremented: u16 = combined + 1;
                self.regs.d = (incremented >> 8) as u8;
                self.regs.e = incremented as u8;
                8
            }, // INC DE
            0x23 => {
                let combined: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let incremented: u16 = combined + 1;
                self.regs.h = (incremented >> 8) as u8;
                self.regs.l = incremented as u8;
                8
            }, // INC HL
            0x33 => {
                self.regs.sp+=1;
                8
            }, // INC SP
            0x04 => {
                self.regs.b = self.regs.b.wrapping_add(1);
                4
            }, // INC B
            0x05 => {
                self.regs.b = self.regs.b.wrapping_sub(1);
                4
            }, // DEC B
            0x06 => {
                self.regs.b = self.next_byte(memory);
                8
            }, // LD B,u8
            0x07 => {
                let carry = self.regs.a & 0x80;
                self.regs.a = self.regs.a << 1 | carry;
                4
            }, // RLCA
            0x08 => {
                let p = self.next_byte(memory);
                let s = self.next_byte(memory);
                let addr: u16 = (s as u16) << 8 | p as u16;
                memory.write(addr, self.regs.sp as u8);
                memory.write(addr+1, (self.regs.sp >> 8) as u8);
                20
            }, // LD (u16),SP
            0x09 => {
                let hl = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let bc = (self.regs.b as u16) << 8 | self.regs.c as u16;
                let sum = hl + bc;
                self.regs.h = (sum >> 8) as u8;
                self.regs.l = sum as u8;
                8
            }, // ADD HL,BC
            0x0A => {
                let addr: u16 = (self.regs.b as u16) << 8 | self.regs.c as u16;
                self.regs.a = memory.read(addr);
                8
            }, // LD A,(BC)
            0x0B => {
                let combined: u16 = (self.regs.b as u16) << 8 | self.regs.c as u16;
                let decremented: u16 = combined - 1;
                self.regs.b = (decremented >> 8) as u8;
                self.regs.c = decremented as u8;
                8
            }, // DEC BC
            0x0C => {
                self.regs.c = self.regs.c.wrapping_add(1);
                4
            }, // INC C
            0x0D => {
                self.regs.c = self.regs.c.wrapping_sub(1);
                4
            }, // DEC C
            0x0E => {
                self.regs.c = self.next_byte(memory);
                8
            }, // LD C,u8
            0x0F => {
                let carry = self.regs.a & 0x01;
                self.regs.a = self.regs.a >> 1 | carry << 7;
                4
            }, // RRCA
            0x10 => { 4 }, // STOP
            0x14 => {
                self.regs.d = self.regs.d.wrapping_add(1);
                4
            }, // INC D

            0x15 => {
                self.regs.d = self.regs.d.wrapping_sub(1);
                4
            }, // DEC D
            0x16 => {
                self.regs.d = self.next_byte(memory);
                8
            }, // LD D,u8
            0x17 => {
                let carry = self.regs.a & 0x80;
                self.regs.a = self.regs.a << 1 | carry;
                4
            }, // RLA
            0x18 => {
                let offset = self.next_byte(memory) as i8;
                self.regs.pc = (self.regs.pc as i32 + offset as i32) as u16;
                12
            }, // JR i8
            0x19 => {
                let hl = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let de = (self.regs.d as u16) << 8 | self.regs.e as u16;
                let sum = hl + de;
                self.regs.h = (sum >> 8) as u8;
                self.regs.l = sum as u8;
                8
            }, // ADD HL,DE
            0x1A => {
                let addr: u16 = (self.regs.d as u16) << 8 | self.regs.e as u16;
                self.regs.a = memory.read(addr);
                8
            }, // LD A,(DE)
            0x1B => {
                let combined: u16 = (self.regs.d as u16) << 8 | self.regs.e as u16;
                let decremented: u16 = combined - 1;
                self.regs.d = (decremented >> 8) as u8;
                self.regs.e = decremented as u8;
                8
            }, // DEC DE
            0x1C => {
                self.regs.e = self.regs.e.wrapping_add(1);
                4
            }, // INC E
            0x1D => {
                self.regs.e = self.regs.e.wrapping_sub(1);
                4
            }, // DEC E
            0x1E => {
                self.regs.e = self.next_byte(memory);
                8
            }, // LD E,u8
            0x1F => {
                let carry = self.regs.a & 0x01;
                self.regs.a = self.regs.a >> 1 | carry << 7;
                4
            }, // RRA
            0x20 => {
                let offset = self.next_byte(memory) as i8;
                if self.regs.f & 0x80 == 0 {
                    self.regs.pc = (self.regs.pc as i32 + offset as i32) as u16;
                    12
                } else {
                    8
                }
            }, // JR NZ,i8
            0x24 => {
                self.regs.h = self.regs.h.wrapping_add(1);
                4
            }, // INC H
            0x25 => {
                self.regs.h = self.regs.h.wrapping_sub(1);
                4
            }, // DEC H
            0x26 => {
                self.regs.h = self.next_byte(memory);
                8
            }, // LD H,u8
            0x27 => {
                let mut a = self.regs.a;
                let mut f = self.regs.f;
                let mut h = self.regs.h;
                let mut l = self.regs.l;
                let mut carry = 0;
                if f & 0x10 == 0x10 {
                    carry = 0x60;
                }
                if f & 0x20 == 0x20 || a > 0x99 {
                    a = a.wrapping_add(0x60);
                    carry |= 0x20;
                }
                if f & 0x20 == 0x20 || (a & 0x0F) > 0x09 {
                    a = a.wrapping_add(0x06);
                }
                f = 0;
                if a & 0x80 == 0x80 {
                    f |= 0x80;
                }
                if a == 0 {
                    f |= 0x80;
                }
                self.regs.a = a;
                self.regs.f = f;
                self.regs.h = h;
                self.regs.l = l;
                4
            }, // DAA
            0x28 => {
                let offset = self.next_byte(memory) as i8;
                if self.regs.f & 0x80 == 0x80 {
                    self.regs.pc = (self.regs.pc as i32 + offset as i32) as u16;
                    12
                } else {
                    8
                }
            }, // JR Z,i8
            0x2C => {
                self.regs.l = self.regs.l.wrapping_add(1);
                4
            }, // INC L
            0x2D => {
                self.regs.l = self.regs.l.wrapping_sub(1);
                4
            }, // DEC L
            0x2E => {
                self.regs.l = self.next_byte(memory);
                8
            }, // LD L,u8
            0x2F => {
                self.regs.a = !self.regs.a;
                4
            }, // CPL
            0x30 => {
                let offset = self.next_byte(memory) as i8;
                if self.regs.f & 0x40 == 0 {
                    self.regs.pc = (self.regs.pc as i32 + offset as i32) as u16;
                    12
                } else {
                    8
                }
            }, // JR NC,i8
            0x34 => {
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let value = memory.read(addr);
                memory.write(addr, value.wrapping_add(1));
                12
            }, // INC (HL)
            0x35 => {
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let value = memory.read(addr);
                memory.write(addr, value.wrapping_sub(1));
                12
            }, // DEC (HL)
            0x36 => {
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let value = self.next_byte(memory);
                memory.write(addr, value);
                12
            }, // LD (HL),u8
            0x37 => {
                self.regs.f = self.regs.f & 0x80 | 0x10;
                4
            }, // SCF
            0x38 => {
                let offset = self.next_byte(memory) as i8;
                if self.regs.f & 0x40 == 0x40 {
                    self.regs.pc = (self.regs.pc as i32 + offset as i32) as u16;
                    12
                } else {
                    8
                }
            }, // JR C,i8
            0x3C => {
                self.regs.a = self.regs.a.wrapping_add(1);
                4
            }, // INC A
            0x3D => {
                self.regs.a = self.regs.a.wrapping_sub(1);
                4
            }, // DEC A
            0x3E => {
                self.regs.a = self.next_byte(memory);
                8
            }, // LD A,u8
            0x3F => {
                self.regs.f = self.regs.f & 0x80;
                4
            }, // CCF
            0x40 => { self.regs.b = self.regs.b; 4 }, // LD B, B
            0x41 => { self.regs.b = self.regs.c; 4 }, // LD B, C
            0x42 => { self.regs.b = self.regs.d; 4 }, // LD B, D
            0x43 => { self.regs.b = self.regs.e; 4 }, // LD B, E
            0x44 => { self.regs.b = self.regs.h; 4 }, // LD B, H
            0x45 => { self.regs.b = self.regs.l; 4 }, // LD B, L
            0x46 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.b = memory.read(addr);
                8
            }, // LD B, (HL)
            0x47 => { self.regs.b = self.regs.a; 4 }, // LD B, A
            0x48 => { self.regs.c = self.regs.b; 4 }, // LD C, B
            0x49 => { self.regs.c = self.regs.c; 4 }, // LD C, C
            0x4A => { self.regs.c = self.regs.d; 4 }, // LD C, D
            0x4B => { self.regs.c = self.regs.e; 4 }, // LD C, E
            0x4C => { self.regs.c = self.regs.h; 4 }, // LD C, H
            0x4D => { self.regs.c = self.regs.l; 4 }, // LD C, L
            0x4E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.c = memory.read(addr);
                8
            }, // LD C, (HL)
            0x4F => { self.regs.c = self.regs.a; 4 }, // LD C, A
            0x50 => { self.regs.d = self.regs.b; 4 }, // LD D, B
            0x51 => { self.regs.d = self.regs.c; 4 }, // LD D, C
            0x52 => { self.regs.d = self.regs.d; 4 }, // LD D, D
            0x53 => { self.regs.d = self.regs.e; 4 }, // LD D, E
            0x54 => { self.regs.d = self.regs.h; 4 }, // LD D, H
            0x55 => { self.regs.d = self.regs.l; 4 }, // LD D, L
            0x56 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.d = memory.read(addr);
                8
            }, // LD D, (HL)
            0x57 => { self.regs.d = self.regs.a; 4 }, // LD D, A
            0x58 => { self.regs.e = self.regs.b; 4 }, // LD E, B
            0x59 => { self.regs.e = self.regs.c; 4 }, // LD E, C
            0x5A => { self.regs.e = self.regs.d; 4 }, // LD E, D
            0x5B => { self.regs.e = self.regs.e; 4 }, // LD E, E
            0x5C => { self.regs.e = self.regs.h; 4 }, // LD E, H
            0x5D => { self.regs.e = self.regs.l; 4 }, // LD E, L
            0x5E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.e = memory.read(addr);
                8
            }, // LD E, (HL)
            0x5F => { self.regs.e = self.regs.a; 4 }, // LD E, A
            0x60 => { self.regs.h = self.regs.b; 4 }, // LD H, B
            0x61 => { self.regs.h = self.regs.c; 4 }, // LD H, C
            0x62 => { self.regs.h = self.regs.d; 4 }, // LD H, D
            0x63 => { self.regs.h = self.regs.e; 4 }, // LD H, E
            0x64 => { self.regs.h = self.regs.h; 4 }, // LD H, H
            0x65 => { self.regs.h = self.regs.l; 4 }, // LD H, L
            0x66 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.h = memory.read(addr);
                8
            }, // LD H, (HL)
            0x67 => { self.regs.h = self.regs.a; 4 }, // LD H, A
            0x68 => { self.regs.l = self.regs.b; 4 }, // LD L, B
            0x69 => { self.regs.l = self.regs.c; 4 }, // LD L, C
            0x6A => { self.regs.l = self.regs.d; 4 }, // LD L, D
            0x6B => { self.regs.l = self.regs.e; 4 }, // LD L, E
            0x6C => { self.regs.l = self.regs.h; 4 }, // LD L, H
            0x6D => { self.regs.l = self.regs.l; 4 }, // LD L, L
            0x6E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.l = memory.read(addr);
                8
            }, // LD L, (HL)
            0x6F => { self.regs.l = self.regs.a; 4 }, // LD L, A
            0x70 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.b);
                8
            }, // LD (HL), B
            0x71 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.c);
                8
            }, // LD (HL), C
            0x72 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.d);
                8
            }, // LD (HL), D
            0x73 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.e);
                8
            }, // LD (HL), E
            0x74 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.h);
                8
            }, // LD (HL), H
            0x75 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.l);
                8
            }, // LD (HL), L
            0x76 => { 4 }, // HALT
            0x77 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                memory.write(addr, self.regs.a);
                8
            }, // LD (HL), A
            0x78 => { self.regs.a = self.regs.b; 4 }, // LD A, B
            0x79 => { self.regs.a = self.regs.c; 4 }, // LD A, C
            0x7A => { self.regs.a = self.regs.d; 4 }, // LD A, D
            0x7B => { self.regs.a = self.regs.e; 4 }, // LD A, E
            0x7C => { self.regs.a = self.regs.h; 4 }, // LD A, H
            0x7D => { self.regs.a = self.regs.l; 4 }, // LD A, L
            0x7E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = memory.read(addr);
                8
            }, // LD A, (HL)
            0x7F => { self.regs.a = self.regs.a; 4 }, // LD A, A
            0x80 => { self.regs.a = self.regs.a.wrapping_add(self.regs.b); 4 }, // ADD A, B
            0x81 => { self.regs.a = self.regs.a.wrapping_add(self.regs.c); 4 }, // ADD A, C
            0x82 => { self.regs.a = self.regs.a.wrapping_add(self.regs.d); 4 }, // ADD A, D
            0x83 => { self.regs.a = self.regs.a.wrapping_add(self.regs.e); 4 }, // ADD A, E
            0x84 => { self.regs.a = self.regs.a.wrapping_add(self.regs.h); 4 }, // ADD A, H
            0x85 => { self.regs.a = self.regs.a.wrapping_add(self.regs.l); 4 }, // ADD A, L
            0x86 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a.wrapping_add(memory.read(addr));
                8
            }, // ADD A, (HL)
            0x87 => { self.regs.a = self.regs.a.wrapping_add(self.regs.a); 4 }, // ADD A, A
            0x88 => { self.regs.a = self.regs.a.wrapping_add(self.regs.b).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, B
            0x89 => { self.regs.a = self.regs.a.wrapping_add(self.regs.c).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, C
            0x8A => { self.regs.a = self.regs.a.wrapping_add(self.regs.d).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, D
            0x8B => { self.regs.a = self.regs.a.wrapping_add(self.regs.e).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, E
            0x8C => { self.regs.a = self.regs.a.wrapping_add(self.regs.h).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, H
            0x8D => { self.regs.a = self.regs.a.wrapping_add(self.regs.l).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, L
            0x8E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a.wrapping_add(memory.read(addr)).wrapping_add(self.regs.f & 0x10);
                8
            }, // ADC A, (HL)
            0x8F => { self.regs.a = self.regs.a.wrapping_add(self.regs.a).wrapping_add(self.regs.f & 0x10); 4 }, // ADC A, A
            0x90 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.b); 4 }, // SUB B
            0x91 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.c); 4 }, // SUB C
            0x92 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.d); 4 }, // SUB D
            0x93 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.e); 4 }, // SUB E
            0x94 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.h); 4 }, // SUB H
            0x95 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.l); 4 }, // SUB L
            0x96 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a.wrapping_sub(memory.read(addr));
                8
            }, // SUB (HL)
            0x97 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.a); 4 }, // SUB A
            0x98 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.b).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, B
            0x99 => { self.regs.a = self.regs.a.wrapping_sub(self.regs.c).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, C
            0x9A => { self.regs.a = self.regs.a.wrapping_sub(self.regs.d).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, D
            0x9B => { self.regs.a = self.regs.a.wrapping_sub(self.regs.e).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, E
            0x9C => { self.regs.a = self.regs.a.wrapping_sub(self.regs.h).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, H
            0x9D => { self.regs.a = self.regs.a.wrapping_sub(self.regs.l).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, L
            0x9E => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a.wrapping_sub(memory.read(addr)).wrapping_sub(self.regs.f & 0x10);
                8
            }, // SBC A, (HL)
            0x9F => { self.regs.a = self.regs.a.wrapping_sub(self.regs.a).wrapping_sub(self.regs.f & 0x10); 4 }, // SBC A, A
            0xA0 => { self.regs.a = self.regs.a & self.regs.b; 4 }, // AND B
            0xA1 => { self.regs.a = self.regs.a & self.regs.c; 4 }, // AND C
            0xA2 => { self.regs.a = self.regs.a & self.regs.d; 4 }, // AND D
            0xA3 => { self.regs.a = self.regs.a & self.regs.e; 4 }, // AND E
            0xA4 => { self.regs.a = self.regs.a & self.regs.h; 4 }, // AND H
            0xA5 => { self.regs.a = self.regs.a & self.regs.l; 4 }, // AND L
            0xA6 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a & memory.read(addr);
                8
            }, // AND (HL)
            0xA7 => { self.regs.a = self.regs.a & self.regs.a; 4 }, // AND A
            0xA8 => { self.regs.a = self.regs.a ^ self.regs.b; 4 }, // XOR B
            0xA9 => { self.regs.a = self.regs.a ^ self.regs.c; 4 }, // XOR C
            0xAA => { self.regs.a = self.regs.a ^ self.regs.d; 4 }, // XOR D
            0xAB => { self.regs.a = self.regs.a ^ self.regs.e; 4 }, // XOR E
            0xAC => { self.regs.a = self.regs.a ^ self.regs.h; 4 }, // XOR H
            0xAD => { self.regs.a = self.regs.a ^ self.regs.l; 4 }, // XOR L
            0xAE => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a ^ memory.read(addr);
                8
            }, // XOR (HL)
            0xAF => { self.regs.a = self.regs.a ^ self.regs.a; 4 }, // XOR A
            0xB0 => { self.regs.a = self.regs.a | self.regs.b; 4 }, // OR B
            0xB1 => { self.regs.a = self.regs.a | self.regs.c; 4 }, // OR C
            0xB2 => { self.regs.a = self.regs.a | self.regs.d; 4 }, // OR D
            0xB3 => { self.regs.a = self.regs.a | self.regs.e; 4 }, // OR E
            0xB4 => { self.regs.a = self.regs.a | self.regs.h; 4 }, // OR H
            0xB5 => { self.regs.a = self.regs.a | self.regs.l; 4 }, // OR L
            0xB6 => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                self.regs.a = self.regs.a | memory.read(addr);
                8
            }, // OR (HL)
            0xB7 => { self.regs.a = self.regs.a | self.regs.a; 4 }, // OR A
            0xB8 => { 
                let result = self.regs.a.wrapping_sub(self.regs.b);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP B
            0xB9 => { 
                let result = self.regs.a.wrapping_sub(self.regs.c);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP C
            0xBA => { 
                let result = self.regs.a.wrapping_sub(self.regs.d);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP D
            0xBB => { 
                let result = self.regs.a.wrapping_sub(self.regs.e);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP E
            0xBC => { 
                let result = self.regs.a.wrapping_sub(self.regs.h);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP H
            0xBD => { 
                let result = self.regs.a.wrapping_sub(self.regs.l);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP L
            0xBE => { 
                let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                let result = self.regs.a.wrapping_sub(memory.read(addr));
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                8
            }, // CP (HL)
            0xBF => { 
                let result = self.regs.a.wrapping_sub(self.regs.a);
                self.regs.f = self.regs.f & 0x80 | 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                4
            }, // CP A
            0xC0 => { 
                if self.regs.f & 0x80 == 0 {
                    let addr: u16 = self.pop(memory);
                    self.regs.pc = addr;
                    20
                } else {
                    8
                }
            }, // RET NZ
            0xC1 => { 
                self.regs.c = self.pop(memory) as u8;
                self.regs.b = self.pop(memory) as u8;
                12
            }, // POP BC
            0xC2 => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0 {
                    self.regs.pc = addr;
                    16
                } else {
                    12
                }
            }, // JP NZ,u16
            0xC3 => { 
                self.regs.pc = self.next_word(memory);
                16
            }, // JP u16
            0xC4 => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0 {
                    self.push(memory, self.regs.pc);
                    self.regs.pc = addr;
                    24
                } else {
                    12
                }
            }, // CALL NZ,u16
            0xC5 => { 
                self.push(memory, self.regs.b as u16);
                self.push(memory, self.regs.c as u16);
                16
            }, // PUSH BC
            0xC6 => { 
                let value = self.next_byte(memory);
                let result = self.regs.a.wrapping_add(value);
                self.regs.f = 0;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                if (self.regs.a & 0x0F) + (value & 0x0F) > 0x0F {
                    self.regs.f |= 0x20;
                }
                self.regs.a = result;
                8
            }, // ADD A,u8
            0xC7 => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x00;
                16
            }, // RST 00H
            0xC8 => { 
                if self.regs.f & 0x80 == 0x80 {
                    let addr = self.pop(memory);
                    self.regs.pc = addr;
                    20
                } else {
                    8
                }
            }, // RET Z
            0xC9 => { 
                self.regs.pc = self.pop(memory);
                16
            }, // RET
            0xCA => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0x80 {
                    self.regs.pc = addr;
                    16
                } else {
                    12
                }
            }, // JP Z,u16
            0xCB => { 
                let opcode = self.next_byte(memory);
                match opcode {
                    0x00 => { 
                        let result = self.regs.b.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.b & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.b = result;
                        8
                    }, // RLC B
                    0x01 => { 
                        let result = self.regs.c.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.c & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.c = result;
                        8
                    }, // RLC C
                    0x02 => { 
                        let result = self.regs.d.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.d & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.d = result;
                        8
                    }, // RLC D
                    0x03 => { 
                        let result = self.regs.e.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.e & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.e = result;
                        8
                    }, // RLC E
                    0x04 => { 
                        let result = self.regs.h.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.h & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.h = result;
                        8
                    }, // RLC H
                    0x05 => { 
                        let result = self.regs.l.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.l & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.l = result;
                        8
                    }, // RLC L
                    0x06 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if memory.read(addr) & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        memory.write(addr, result);
                        16
                    }, // RLC (HL)
                    0x07 => { 
                        let result = self.regs.a.rotate_left(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.a & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.a = result;
                        8
                    }, // RLC A
                    0x08 => { 
                        let result = self.regs.b.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.b & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.b = result;
                        8
                    }, // RRC B
                    0x09 => { 
                        let result = self.regs.c.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.c & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.c = result;
                        8
                    }, // RRC C
                    0x0A => { 
                        let result = self.regs.d.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.d & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.d = result;
                        8
                    }, // RRC D
                    0x0B => { 
                        let result = self.regs.e.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.e & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.e = result;
                        8
                    }, // RRC E
                    0x0C => { 
                        let result = self.regs.h.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.h & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.h = result;
                        8
                    }, // RRC H
                    0x0D => { 
                        let result = self.regs.l.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.l & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.l = result;
                        8
                    }, // RRC L
                    0x0E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if memory.read(addr) & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        memory.write(addr, result);
                        16
                    }, // RRC (HL)
                    0x0F => { 
                        let result = self.regs.a.rotate_right(1);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.a & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.a = result;
                        8
                    }, // RRC A
                    0x10 => { 
                        let result = self.regs.b.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL B
                    0x11 => { 
                        let result = self.regs.c.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL C
                    0x12 => { 
                        let result = self.regs.d.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL D
                    0x13 => { 
                        let result = self.regs.e.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL E
                    0x14 => { 
                        let result = self.regs.h.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL H
                    0x15 => { 
                        let result = self.regs.l.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL L
                    0x16 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        memory.write(addr, result);
                        16
                    }, // RL (HL)
                    0x17 => { 
                        let result = self.regs.a.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RL A
                    0x18 => { 
                        let result = self.regs.b.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR B
                    0x19 => { 
                        let result = self.regs.c.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR C
                    0x1A => { 
                        let result = self.regs.d.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR D
                    0x1B => { 
                        let result = self.regs.e.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR E
                    0x1C => { 
                        let result = self.regs.h.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR H
                    0x1D => { 
                        let result = self.regs.l.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR L
                    0x1E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        memory.write(addr, result);
                        16
                    }, // RR (HL)
                    0x1F => { 
                        let result = self.regs.a.rotate_right(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // RR A
                    0x20 => { 
                        let result = self.regs.b.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.b & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.b = result;
                        8
                    }, // SLA B
                    0x21 => { 
                        let result = self.regs.c.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.c & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.c = result;
                        8
                    }, // SLA C
                    0x22 => { 
                        let result = self.regs.d.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.d & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.d = result;
                        8
                    }, // SLA D
                    0x23 => { 
                        let result = self.regs.e.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.e & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.e = result;
                        8
                    }, // SLA E
                    0x24 => { 
                        let result = self.regs.h.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.h & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.h = result;
                        8
                    }, // SLA H
                    0x25 => { 
                        let result = self.regs.l.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.l & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.l = result;
                        8
                    }, // SLA L
                    0x26 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if memory.read(addr) & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        memory.write(addr, result);
                        16
                    }, // SLA (HL)
                    0x27 => { 
                        let result = self.regs.a.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.a & 0x80 == 0x80 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.a = result;
                        8
                    }, // SLA A
                    0x28 => { 
                        let result = self.regs.b.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.b & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.b = result;
                        8
                    }, // SRA B
                    0x29 => { 
                        let result = self.regs.c.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.c & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.c = result;
                        8
                    }, // SRA C
                    0x2A => { 
                        let result = self.regs.d.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.d & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.d = result;
                        8
                    }, // SRA D
                    0x2B => { 
                        let result = self.regs.e.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.e & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.e = result;
                        8
                    }, // SRA E
                    0x2C => { 
                        let result = self.regs.h.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.h & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.h = result;
                        8
                    }, // SRA H
                    0x2D => { 
                        let result = self.regs.l.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.l & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.l = result;
                        8
                    }, // SRA L
                    0x2E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if memory.read(addr) & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        memory.write(addr, result);
                        16
                    }, // SRA (HL)
                    0x2F => { 
                        let result = self.regs.a.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = 0;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.a & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.a = result;
                        8
                    }, // SRA A
                    0x30 => { 
                        let result = self.regs.b.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP B
                    0x31 => { 
                        let result = self.regs.c.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP C
                    0x32 => { 
                        let result = self.regs.d.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP D
                    0x33 => { 
                        let result = self.regs.e.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP E
                    0x34 => { 
                        let result = self.regs.h.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP H
                    0x35 => { 
                        let result = self.regs.l.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP L
                    0x36 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        memory.write(addr, result);
                        16
                    }, // SWAP (HL)
                    0x37 => { 
                        let result = self.regs.a.rotate_left_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SWAP A
                    0x38 => { 
                        let result = self.regs.b.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL B
                    0x39 => { 
                        let result = self.regs.c.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL C
                    0x3A => { 
                        let result = self.regs.d.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL D
                    0x3B => { 
                        let result = self.regs.e.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL E
                    0x3C => { 
                        let result = self.regs.h.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL H
                    0x3D => { 
                        let result = self.regs.l.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        8
                    }, // SRL L
                    0x3E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        memory.write(addr, result);
                        16
                    }, // SRL (HL)
                    0x3F => { 
                        //let result = self.regs.a.rotate_right_through_carry(1, self.regs.f & 0x10 == 0x10);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }

                        self.regs.a<<1;
                        8
                    }, // SRL A
                    0x40 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,B
                    0x41 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,C
                    0x42 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,D
                    0x43 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,E
                    0x44 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,H
                    0x45 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,L
                    0x46 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 0,(HL)
                    0x47 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 0,A
                    0x48 => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,B
                    0x49 => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,C
                    0x4A => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,D
                    0x4B => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,E
                    0x4C => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,H
                    0x4D => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,L
                    0x4E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 1,(HL)
                    0x4F => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 1,A
                    0x50 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,B
                    0x51 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,C
                    0x52 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,D
                    0x53 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,E
                    0x54 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,H
                    0x55 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,L
                    0x56 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 2,(HL)
                    0x57 => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 2,A
                    0x58 => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,B
                    0x59 => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,C
                    0x5A => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,D
                    0x5B => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,E
                    0x5C => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,H
                    0x5D => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,L
                    0x5E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 3,(HL)
                    0x5F => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 3,A
                    0x60 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,B
                    0x61 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,C
                    0x62 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,D
                    0x63 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,E
                    0x64 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,H
                    0x65 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,L
                    0x66 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 4,(HL)
                    0x67 => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 4,A
                    0x68 => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,B
                    0x69 => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,C
                    0x6A => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,D
                    0x6B => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,E
                    0x6C => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,H
                    0x6D => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,L
                    0x6E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 5,(HL)
                    0x6F => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 5,A
                    0x70 => { 
                        let result = self.regs.b.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.b & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.b = result;
                        8
                    }, // BIT 6,B
                    0x71 => { 
                        let result = self.regs.c.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.c & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.c = result;
                        8
                    }, // BIT 6,C
                    0x72 => { 
                        let result = self.regs.d.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.d & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.d = result;
                        8
                    }, // BIT 6,D
                    0x73 => { 
                        let result = self.regs.e.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.e & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.e = result;
                        8
                    }, // BIT 6,E
                    0x74 => { 
                        let result = self.regs.h.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.h & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.h = result;
                        8
                    }, // BIT 6,H
                    0x75 => { 
                        let result = self.regs.l.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.l & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.l = result;
                        8
                    }, // BIT 6,L
                    0x76 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        let result = memory.read(addr).rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if memory.read(addr) & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        memory.write(addr, result);
                        16
                    }, // BIT 6,(HL)
                    0x77 => { 
                        let result = self.regs.a.rotate_left(1);
                        self.regs.f = self.regs.f & 0x80 | 0x40;
                        if result == 0 {
                            self.regs.f |= 0x80;
                        }
                        if self.regs.a & 0x01 == 0x01 {
                            self.regs.f |= 0x20;
                        }
                        self.regs.a = result;
                        8
                    }, // BIT 6,A
                    0x78 => { 
                        self.regs.b = self.regs.b.rotate_left(1);
                        8
                    }, // BIT 7,B

                    0x79 => { 
                        self.regs.c = self.regs.c.rotate_left(1);
                        8
                    }, // BIT 7,C
                    0x7A => { 
                        self.regs.d = self.regs.d.rotate_left(1);
                        8
                    }, // BIT 7,D
                    0x7B => { 
                        self.regs.e = self.regs.e.rotate_left(1);
                        8
                    }, // BIT 7,E
                    0x7C => { 
                        self.regs.h = self.regs.h.rotate_left(1);
                        8
                    }, // BIT 7,H

                    0x7D => { 
                        self.regs.l = self.regs.l.rotate_left(1);
                        8
                    }, // BIT 7,L
                    0x7E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.read(addr);
                        16
                    }, // BIT 7,(HL)
                    0x7F => { 
                        self.regs.a = self.regs.a.rotate_left(1);
                        8
                    }, // BIT 7,A
                    0x80 => { 
                        self.regs.b = self.regs.b & 0xFE;
                        8
                    }, // RES 0,B
                    0x81 => { 
                        self.regs.b = self.regs.b & 0xFD;
                        8
                    }, // RES 0,C
                    0x82 => { 
                        self.regs.b = self.regs.b & 0xFB;
                        8
                    }, // RES 0,D
                    0x83 => { 
                        self.regs.b = self.regs.b & 0xF7;
                        8
                    }, // RES 0,E
                    0x84 => { 
                        self.regs.b = self.regs.b & 0xEF;
                        8
                    }, // RES 0,H
                    0x85 => { 
                        self.regs.b = self.regs.b & 0xDF;
                        8
                    }, // RES 0,L
                    0x86 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xFE);
                        16
                    }, // RES 0,(HL)
                    0x87 => { 
                        self.regs.b = self.regs.b & 0xBF;
                        8
                    }, // RES 0,A
                    0x88 => { 
                        self.regs.c = self.regs.c & 0xFE;
                        8
                    }, // RES 1,B
                    0x89 => { 
                        self.regs.c = self.regs.c & 0xFD;
                        8
                    }, // RES 1,C
                    0x8A => { 
                        self.regs.c = self.regs.c & 0xFB;
                        8
                    }, // RES 1,D
                    0x8B => { 
                        self.regs.c = self.regs.c & 0xF7;
                        8
                    }, // RES 1,E
                    0x8C => { 
                        self.regs.c = self.regs.c & 0xEF;
                        8
                    }, // RES 1,H
                    0x8D => { 
                        self.regs.c = self.regs.c & 0xDF;
                        8
                    }, // RES 1,L
                    0x8E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xFD);
                        16
                    }, // RES 1,(HL)
                    0x8F => { 
                        self.regs.c = self.regs.c & 0xBF;
                        8
                    }, // RES 1,A
                    0x90 => { 
                        self.regs.d = self.regs.d & 0xFE;
                        8
                    }, // RES 2,B
                    0x91 => { 
                        self.regs.d = self.regs.d & 0xFD;
                        8
                    }, // RES 2,C
                    0x92 => { 
                        self.regs.d = self.regs.d & 0xFB;
                        8
                    }, // RES 2,D
                    0x93 => { 
                        self.regs.d = self.regs.d & 0xF7;
                        8
                    }, // RES 2,E
                    0x94 => { 
                        self.regs.d = self.regs.d & 0xEF;
                        8
                    }, // RES 2,H
                    0x95 => { 
                        self.regs.d = self.regs.d & 0xDF;
                        8
                    }, // RES 2,L
                    0x96 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 2,(HL)
                    0x97 => { 
                        self.regs.d = self.regs.d & 0xBF;
                        8
                    }, // RES 2,A
                    0x98 => { 
                        self.regs.e = self.regs.e & 0xFE;
                        8
                    }, // RES 3,B
                    0x99 => { 
                        self.regs.e = self.regs.e & 0xFD;
                        8
                    }, // RES 3,C
                    0x9A => { 
                        self.regs.e = self.regs.e & 0xFB;
                        8
                    }, // RES 3,D
                    0x9B => { 
                        self.regs.e = self.regs.e & 0xF7;
                        8
                    }, // RES 3,E
                    0x9C => { 
                        self.regs.e = self.regs.e & 0xEF;
                        8
                    }, // RES 3,H
                    0x9D => { 
                        self.regs.e = self.regs.e & 0xDF;
                        8
                    }, // RES 3,L
                    0x9E => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xF7);
                        16
                    }, // RES 3,(HL)
                    0x9F => { 
                        self.regs.e = self.regs.e & 0xBF;
                        8
                    }, // RES 3,A
                    0xA0 => { 
                        self.regs.h = self.regs.h & 0xFE;
                        8
                    }, // RES 4,B
                    0xA1 => { 
                        self.regs.h = self.regs.h & 0xFD;
                        8
                    }, // RES 4,C
                    0xA2 => { 
                        self.regs.h = self.regs.h & 0xFB;
                        8
                    }, // RES 4,D
                    0xA3 => { 
                        self.regs.h = self.regs.h & 0xF7;
                        8
                    }, // RES 4,E
                    0xA4 => { 
                        self.regs.h = self.regs.h & 0xEF;
                        8
                    }, // RES 4,H
                    0xA5 => { 
                        self.regs.h = self.regs.h & 0xDF;
                        8
                    }, // RES 4,L
                    0xA6 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xEF);
                        16
                    }, // RES 4,(HL)
                    0xA7 => { 
                        self.regs.h = self.regs.h & 0xBF;
                        8
                    }, // RES 4,A
                    0xA8 => { 
                        self.regs.l = self.regs.l & 0xFE;
                        8
                    }, // RES 5,B
                    0xA9 => { 
                        self.regs.l = self.regs.l & 0xFD;
                        8
                    }, // RES 5,C
                    0xAA => { 
                        self.regs.l = self.regs.l & 0xFB;
                        8
                    }, // RES 5,D
                    0xAB => { 
                        self.regs.l = self.regs.l & 0xF7;
                        8
                    }, // RES 5,E
                    0xAC => { 
                        self.regs.l = self.regs.l & 0xEF;
                        8
                    }, // RES 5,H
                    0xAD => { 
                        self.regs.l = self.regs.l & 0xDF;
                        8
                    }, // RES 5,L
                    0xAE => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xDF);
                        16
                    }, // RES 5,(HL)
                    0xAF => { 
                        self.regs.l = self.regs.l & 0xBF;
                        8
                    }, // RES 5,A
                    0xB0 => { 
                        self.regs.a = self.regs.a & 0xFE;
                        8
                    }, // RES 6,B
                    0xB1 => { 
                        self.regs.a = self.regs.a & 0xFD;
                        8
                    }, // RES 6,C
                    0xB2 => { 
                        self.regs.a = self.regs.a & 0xFB;
                        8
                    }, // RES 6,D
                    0xB3 => { 
                        self.regs.a = self.regs.a & 0xF7;
                        8
                    }, // RES 6,E
                    0xB4 => { 
                        self.regs.a = self.regs.a & 0xEF;
                        8
                    }, // RES 6,H
                    0xB5 => { 
                        self.regs.a = self.regs.a & 0xDF;
                        8
                    }, // RES 6,L
                    0xB6 => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xEF);
                        16
                    }, // RES 6,(HL)
                    0xB7 => { 
                        self.regs.a = self.regs.a & 0xBF;
                        8
                    }, // RES 6,A
                    0xB8 => { 
                        self.regs.a = self.regs.a & 0xFE;
                        8
                    }, // RES 7,B
                    0xB9 => { 
                        self.regs.a = self.regs.a & 0xFD;
                        8
                    }, // RES 7,C
                    0xBA => { 
                        self.regs.a = self.regs.a & 0xFB;
                        8
                    }, // RES 7,D
                    0xBB => { 
                        self.regs.a = self.regs.a & 0xF7;
                        8
                    }, // RES 7,E
                    0xBC => { 
                        self.regs.a = self.regs.a & 0xEF;
                        8
                    }, // RES 7,H
                    0xBD => { 
                        self.regs.a = self.regs.a & 0xDF;
                        8
                    }, // RES 7,L
                    0xBE => { 
                        let addr: u16 = (self.regs.h as u16) << 8 | self.regs.l as u16;
                        memory.write(addr, memory.read(addr) & 0xEF);
                        16
                    }, // RES 7,(HL)
                    0xBF => { 
                        self.regs.a = self.regs.a & 0xBF;
                        8
                    }, // RES 7,A
                    0xC0 => {
                        // RES 0,B
                        self.regs.b = self.regs.b & 0xFE;
                        8
                    }, // RES 0,C
                    0xC1 => {
                        self.regs.b = self.regs.b & 0xFD;
                        8
                    }, // RES 0,D
                    0xC2 => {
                        self.regs.b = self.regs.b & 0xFB;
                        8
                    }, // RES 0,E
                    0xC3 => {
                        self.regs.b = self.regs.b & 0xF7;
                        8
                    }, // RES 0,H
                    0xC4 => {
                        self.regs.h = self.regs.b | 0x01;
                        8
                    }, // SET 0,H
                    0xC5 => {
                        self.regs.l = self.regs.l | 0x01;
                        8
                    }, // SET 0,L
                    0xC6 => {
                        self.regs.b = self.regs.b & 0xBF;
                        8
                    }, // RES 0,A
                    0xC7 => {
                        self.regs.c = self.regs.c & 0xFE;
                        8
                    }, // RES 1,B
                    0xC8 => {
                        self.regs.c = self.regs.c & 0xFD;
                        8
                    }, // RES 1,C
                    0xC9 => {
                        self.regs.c = self.regs.c & 0xFB;
                        8
                    }, // RES 1,D
                    0xCA => {
                        self.regs.c = self.regs.c & 0xF7;
                        8
                    }, // RES 1,E
                    0xCB => {
                        self.regs.c = self.regs.c & 0xEF;
                        8
                    }, // RES 1,H
                    0xCC => {
                        self.regs.c = self.regs.c & 0xDF;
                        8
                    }, // RES 1,L
                    0xCD => {
                        memory.write(addr, memory.read(addr) & 0xF7);
                        16
                    }, // RES 1,(HL)
                    0xCE => {
                        self.regs.c = self.regs.c & 0xBF;
                        8
                    }, // RES 1,A
                    0xCF => {
                        self.regs.d = self.regs.d & 0xFE;
                        8
                    }, // RES 2,B
                    0xD0 => {
                        self.regs.d = self.regs.d & 0xFD;
                        8
                    }, // RES 2,C
                    0xD1 => {
                        self.regs.d = self.regs.d & 0xFB;
                        8
                    }, // RES 2,D
                    0xD2 => {
                        self.regs.d = self.regs.d & 0xF7;
                        8
                    }, // RES 2,E
                    0xD3 => {
                        self.regs.d = self.regs.d & 0xEF;
                        8
                    }, // RES 2,H
                    0xD4 => {
                        self.regs.d = self.regs.d & 0xDF;
                        8
                    }, // RES 2,L
                    0xD5 => {
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 2,(HL)
                    0xD6 => {
                        self.regs.d = self.regs.d & 0xBF;
                        8
                    }, // RES 2,A
                    0xD7 => {
                        self.regs.e = self.regs.e & 0xFE;
                        8
                    }, // RES 3,B
                    0xD8 => {
                        self.regs.e = self.regs.e & 0xFD;
                        8
                    }, // RES 3,C
                    0xD9 => {
                        self.regs.e = self.regs.e & 0xFB;
                        8
                    }, // RES 3,D
                    0xDA => {
                        self.regs.e = self.regs.e & 0xF7;
                        8
                    }, // RES 3,E
                    0xDB => {
                        self.regs.e = self.regs.e & 0xEF;
                        8
                    }, // RES 3,H
                    0xDC => {
                        self.regs.e = self.regs.e & 0xDF;
                        8
                    }, // RES 3,L
                    0xDD => {
                        memory.write(addr, memory.read(addr) & 0xF7);
                        16
                    }, // RES 3,(HL)
                    0xDE => {
                        self.regs.e = self.regs.e & 0xBF;
                        8
                    }, // RES 3,A
                    0xDF => {
                        self.regs.h = self.regs.h & 0xFE;
                        8
                    }, // RES 4,B
                    0xE0 => {
                        self.regs.h = self.regs.h & 0xFD;
                        8
                    }, // RES 4,C
                    0xE1 => {
                        self.regs.h = self.regs.h & 0xFB;
                        8
                    }, // RES 4,D
                    0xE2 => {
                        self.regs.h = self.regs.h & 0xF7;
                        8
                    }, // RES 4,E
                    0xE3 => {
                        self.regs.h = self.regs.h & 0xEF;
                        8
                    }, // RES 4,H
                    0xE4 => {
                        self.regs.h = self.regs.h & 0xDF;
                        8
                    }, // RES 4,L
                    0xE5 => {
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 4,(HL)
                    0xE6 => {
                        self.regs.h = self.regs.h & 0xBF;
                        8
                    }, // RES 4,A
                    0xE7 => {
                        self.regs.l = self.regs.l & 0xFE;
                        8
                    }, // RES 5,B
                    0xE8 => {
                        self.regs.l = self.regs.l & 0xFD;
                        8
                    }, // RES 5,C
                    0xE9 => {
                        self.regs.l = self.regs.l & 0xFB;
                        8
                    }, // RES 5,D
                    0xEA => {
                        self.regs.l = self.regs.l & 0xF7;
                        8
                    }, // RES 5,E
                    0xEB => {
                        self.regs.l = self.regs.l & 0xEF;
                        8
                    }, // RES 5,H
                    0xEC => {
                        self.regs.l = self.regs.l & 0xDF;
                        8
                    }, // RES 5,L
                    0xED => {
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 5,(HL)
                    0xEE => {
                        self.regs.l = self.regs.l & 0xBF;
                        8
                    }, // RES 5,A
                    0xEF => {
                        self.regs.a = self.regs.a & 0xFE;
                        8
                    }, // RES 6,B
                    0xF0 => {
                        self.regs.a = self.regs.a & 0xFD;
                        8
                    }, // RES 6,C
                    0xF1 => {
                        self.regs.a = self.regs.a & 0xFB;
                        8
                    }, // RES 6,D
                    0xF2 => {
                        self.regs.a = self.regs.a & 0xF7;
                        8
                    }, // RES 6,E
                    0xF3 => {
                        self.regs.a = self.regs.a & 0xEF;
                        8
                    }, // RES 6,H
                    0xF4 => {
                        self.regs.a = self.regs.a & 0xDF;
                        8
                    }, // RES 6,L
                    0xF5 => {
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 6,(HL)
                    0xF6 => {
                        self.regs.a = self.regs.a & 0xBF;
                        8
                    }, // RES 6,A
                    0xF7 => {
                        self.regs.a = self.regs.a & 0xFE;
                        8
                    }, // RES 7,B
                    0xF8 => {
                        self.regs.a = self.regs.a & 0xFD;
                        8
                    }, // RES 7,C
                    0xF9 => {
                        self.regs.a = self.regs.a & 0xFB;
                        8
                    }, // RES 7,D
                    0xFA => {
                        self.regs.a = self.regs.a & 0xF7;
                        8
                    }, // RES 7,E
                    0xFB => {
                        self.regs.a = self.regs.a & 0xEF;
                        8
                    }, // RES 7,H
                    0xFC => {
                        self.regs.a = self.regs.a & 0xDF;
                        8
                    }, // RES 7,L
                    0xFD => {
                        memory.write(addr, memory.read(addr) & 0xFB);
                        16
                    }, // RES 7,(HL)
                    0xFE => {
                        self.regs.a = self.regs.a & 0xBF;
                        8
                    }, // RES 7,A
                    0xFF => {
                        self.regs.a = self.regs.a & 0x7F;
                        8
                    } // RES 7,A
                    
                }
            }
            0xCC => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0x80 {
                    self.push(memory, self.regs.pc);
                    self.regs.pc = addr;
                    24
                } else {
                    12
                }
            }, // CALL Z,u16
            0xCD => { 
                let addr = self.next_word(memory);
                self.push(memory, self.regs.pc);
                self.regs.pc = addr;
                24
            }, // CALL u16
            0xCE => { 
                let value = self.next_byte(memory);
                let result = self.regs.a.wrapping_add(value).wrapping_add(self.regs.f & 0x10);
                self.regs.f = 0;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                if (self.regs.a & 0x0F) + (value & 0x0F) > 0x0F {
                    self.regs.f |= 0x20;
                }
                self.regs.a = result;
                8
            }, // ADC A,u8
            0xCF => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x08;
                16
            }, // RST 08H
            0xD0 => { 
                if self.regs.f & 0x80 == 0 {
                    let addr = self.pop(memory);
                    self.regs.pc = addr;
                    20
                } else {
                    8
                }
            }, // RET NC
            0xD1 => { 
                self.regs.e = self.pop(memory) as u8;
                self.regs.d = self.pop(memory) as u8;
                12
            }, // POP DE
            0xD2 => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0 {
                    self.regs.pc = addr;
                    16
                } else {
                    12
                }
            }, // JP NC,u16
            0xD4 => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0 {
                    self.push(memory, self.regs.pc);
                    self.regs.pc = addr;
                    24
                } else {
                    12
                }
            }, // CALL NC,u16
            0xD5 => { 
                self.push(memory, self.regs.d as u16);
                self.push(memory, self.regs.e as u16);
                16
            }, // PUSH DE
            0xD6 => { 
                let value = self.next_byte(memory);
                let result = self.regs.a.wrapping_sub(value);
                self.regs.f = 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                if (self.regs.a & 0x0F) < (value & 0x0F) {
                    self.regs.f |= 0x20;
                }
                self.regs.a = result;
                8
            }, // SUB u8
            0xD7 => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x10;
                16
            }, // RST 10H
            0xD8 => { 
                if self.regs.f & 0x80 == 0x80 {
                    let addr = self.pop(memory);
                    self.regs.pc = addr;
                    20
                } else {
                    8
                }
            }, // RET C
            0xD9 => { 
                self.regs.pc = self.pop(memory);
                16
            }, // RETI
            0xDA => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0x80 {
                    self.regs.pc = addr;
                    16
                } else {
                    12
                }
            }, // JP C,u16
            0xDC => { 
                let addr = self.next_word(memory);
                if self.regs.f & 0x80 == 0x80 {
                    self.push(memory, self.regs.pc);
                    self.regs.pc = addr;
                    24
                } else {
                    12
                }
            }, // CALL C,u16
            0xDE => { 
                let value = self.next_byte(memory);
                let result = self.regs.a.wrapping_sub(value).wrapping_sub(self.regs.f & 0x10);
                self.regs.f = 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                if (self.regs.a & 0x0F) < (value & 0x0F) {
                    self.regs.f |= 0x20;
                }
                self.regs.a = result;
                8
            }, // SBC A,u8
            0xDF => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x18;
                16
            }, // RST 18H
            0xE0 => { 
                let addr = 0xFF00 + self.next_byte(memory) as u16;
                memory.write(addr, self.regs.a);
                12
            }, // LDH (u8),A
            0xE1 => { 
                self.regs.l = self.pop(memory) as u8;
                self.regs.h = self.pop(memory) as u8;
                12
            }, // POP HL
            0xE2 => { 
                let addr = 0xFF00 + self.regs.c as u16;
                memory.write(addr, self.regs.a);
                8
            }, // LD (C),A
            0xE5 => { 
                self.push(memory, self.regs.h as u16);
                self.push(memory, self.regs.l as u16);
                16
            }, // PUSH HL
            0xE6 => { 
                let value = self.next_byte(memory);
                self.regs.a = self.regs.a & value;
                8
            }, // AND u8
            0xE7 => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x20;
                16
            }, // RST 20H
            0xE8 => { 
                let value = self.next_byte(memory) as u8 as u16;
                let result = self.regs.sp.wrapping_add(value);
                self.regs.f = 0;
                if (self.regs.sp & 0x0F) + (value & 0x0F) > 0x0F {
                    self.regs.f |= 0x20;
                }
                if (self.regs.sp & 0xFF) + (value & 0xFF) > 0xFF {
                    self.regs.f |= 0x10;
                }
                self.regs.sp = result as u16;
                16
            }, // ADD SP,i8
            0xE9 => { 
                self.regs.pc = (self.regs.h as u16) << 8 | self.regs.l as u16;
                4
            }, // JP (HL)
            0xEA => { 
                let addr = self.next_word(memory);
                memory.write(addr, self.regs.a);
                16
            }, // LD (u16),A
            0xEE => { 
                let value = self.next_byte(memory);
                self.regs.a = self.regs.a ^ value;
                8
            }, // XOR u8
            0xEF => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x28;
                16
            }, // RST 28H
            0xF0 => { 
                let addr = 0xFF00 + self.next_byte(memory) as u16;
                self.regs.a = memory.read(addr);
                12
            }, // LDH A,(u8)
            0xF1 => { 
                self.regs.a = self.pop(memory) as u8;
                self.regs.f = self.pop(memory) as u8;
                12
            }, // POP AF
            0xF2 => { 
                let addr = 0xFF00 + self.regs.c as u16;
                self.regs.a = memory.read(addr);
                8
            }, // LD A,(C)
            0xF3 => { 
                self.fs.ime = false;
                4
            }, // DI
            0xF5 => { 
                self.push(memory, self.regs.a as u16);
                self.push(memory, self.regs.f as u16);
                16
            }, // PUSH AF
            0xF6 => { 
                let value = self.next_byte(memory);
                self.regs.a = self.regs.a | value;
                8
            }, // OR u8
            0xF7 => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x30;
                16
            }, // RST 30H
            0xF8 => { 
                let value = self.next_byte(memory) as u8 as u16;
                let result = self.regs.sp.wrapping_add(value);
                self.regs.f = 0;
                if (self.regs.sp & 0x0F) + (value & 0x0F) > 0x0F {
                    self.regs.f |= 0x20;
                }
                if (self.regs.sp & 0xFF) + (value & 0xFF) > 0xFF {
                    self.regs.f |= 0x10;
                }
                self.regs.sp = result as u16;
                16
            }, // LD HL,SP+i8
            0xF9 => { 
                self.regs.sp = (self.regs.h as u16) << 8 | self.regs.l as u16;
                8
            }, // LD SP,HL
            0xFA => { 
                let addr = self.next_word(memory);
                self.regs.a = memory.read(addr);
                16
            }, // LD A,(u16)
            0xFB => { 
                self.fs.ime = true;
                4
            }, // EI
            0xFE => { 
                let value = self.next_byte(memory);
                let result = self.regs.a.wrapping_sub(value);
                self.regs.f = 0x40;
                if result == 0 {
                    self.regs.f |= 0x80;
                }
                if (self.regs.a & 0x0F) < (value & 0x0F) {
                    self.regs.f |= 0x20;
                }
                8
            }, // CP u8
            0xFF => { 
                self.push(memory, self.regs.pc);
                self.regs.pc = 0x38;
                16
            }, // RST 38H
            _ => panic!("Unknown Opcode: ${:02X} @ ${:04X}", opcode, self.regs.pc)
        }
    }
}