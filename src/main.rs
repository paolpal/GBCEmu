mod memory;

use memory::wram::WRam;
use memory::rom::Rom;

fn main() {
    let mut ram = WRam::new();
    let rom = Rom::load("resources/rom.gbc".to_string());
    let addr : u16 = 456;
    let value : u8 = 8;
    println!("RAM {:x} : {:8b} " , addr, ram.read(addr));
    ram.write(addr, value);
    println!("RAM {:x} : {:8b} " , addr, ram.read(addr));
    println!("ROM {:x} : {:x} " , addr, rom.read(addr));
}
