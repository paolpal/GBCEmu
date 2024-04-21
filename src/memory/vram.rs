const VRAM_SIZE : usize = 32768;

pub struct VRam {
    bytes : Vec<u8>,
}

impl VRam {
    pub fn new() -> VRam {
        VRam {bytes: [0;VRAM_SIZE].to_vec() }
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

    pub fn write(&mut self, addr: u16, value: u8) {
        self.bytes[addr as usize] = value;
    }
}