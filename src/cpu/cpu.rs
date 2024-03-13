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

pub struct CPU {
    pub regs : Registers,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
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
            }
        }
    }

    pub fn print_state(&self) {
        println!("{}", self.regs);
    }

    fn next_byte(&mut self, memory: &WRam) -> u8 {
        let byte = memory.read(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        byte
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
            0x78 => { self.regs.a = self.regs.b; 4 }, // LD A, B
            0x79 => { self.regs.a = self.regs.c; 4 }, // LD A, C
            // repeat for every instruction...
            _ => panic!("Unknown Opcode: ${:02X} @ ${:04X}", opcode, self.regs.pc)
        }
    } 
}