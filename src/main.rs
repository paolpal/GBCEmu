mod memory;
mod cpu;

use memory::wram::WRam;
use memory::rom::Rom;
use cpu::cpu::Processor;

fn main() {
    let mut ram = WRam::new();
    let mut cpu = Processor::new();
    let rom = Rom::load(String::from("resources/rom.gbc"));
    let addr : u16 = 456;
    let value : u8 = 8;
    println!("RAM {:x} : {:08b} " , addr, ram.read(addr));
    ram.write(addr, value);
    println!("RAM {:x} : {:08b} " , addr, ram.read(addr));
    println!("ROM {:x} : {:x} " , addr, rom.read(addr));
    let nb = cpu.step(&mut ram);
    println!("{}",nb);
    cpu.print_state();
}
