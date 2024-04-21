const HRAM_SIZE : usize = 128;

pub struct HRam {
    bytes : Vec<u8>,
}

impl HRam {
    pub fn new() -> HRam {
        HRam {bytes: [0;HRAM_SIZE].to_vec() }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.bytes[addr as usize] = value;
    }
}