const WRAM_SIZE : usize = 32768;

pub struct WRam {
    bytes : Vec<u8>,
}

impl WRam {
    pub fn new() -> WRam {
        WRam {bytes: [0;WRAM_SIZE].to_vec() }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.bytes[addr as usize] = value;
    }
}