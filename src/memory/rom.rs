use std::fs::File;
use std::io::Read;

pub struct Rom {
    bytes : Vec<u8>,
}

impl Rom {

    pub fn load(path: String) -> Rom {
        let mut buffer = Vec::new();
        let mut file = File::open(path).expect("Invalid ROM path");
        file.read_to_end(&mut buffer).expect("Unable to read ROM");
        Rom {bytes: buffer}
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.bytes[addr as usize]
    }

}