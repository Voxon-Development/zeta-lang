pub struct ByteWriter {
    pub buffer: Vec<u8>,
}

impl ByteWriter {
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }
    
    pub fn extend(&mut self, other: Vec<u8>) {
        self.buffer.extend(other);
    }

    pub fn write_u8(&mut self, value: u8) {
        self.buffer.push(value);
    }

    pub fn write_u16(&mut self, value: u16) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    pub fn write_u32(&mut self, value: u32) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    pub fn write_i64(&mut self, value: i64) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    pub fn write_string(&mut self, value: &str) {
        let bytes = value.as_bytes();
        self.write_u16(bytes.len() as u16); // allows up to 65535-byte names
        self.buffer.extend_from_slice(bytes);
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buffer
    }
}

pub struct ByteReader<'a> {
    pub buffer: &'a [u8],
    pub ip: usize,
}

impl<'a> ByteReader<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        Self { buffer, ip: 0 }
    }

    pub fn read_u8(&mut self) -> u8 {
        let val = self.buffer[self.ip];
        self.ip += 1;
        val
    }

    pub fn read_u16(&mut self) -> u16 {
        let val = u16::from_le_bytes(self.buffer[self.ip..self.ip+2].try_into().unwrap());
        self.ip += 2;
        val
    }

    pub fn read_u32(&mut self) -> u32 {
        let val = u32::from_le_bytes(self.buffer[self.ip..self.ip+4].try_into().unwrap());
        self.ip += 4;
        val
    }

    pub fn read_i64(&mut self) -> i64 {
        let val = i64::from_le_bytes(self.buffer[self.ip..self.ip+8].try_into().unwrap());
        self.ip += 8;
        val
    }

    pub fn read_string(&mut self) -> String {
        let len = self.read_u16() as usize;
        let s = &self.buffer[self.ip..self.ip+len];
        self.ip += len;
        String::from_utf8_lossy(s).to_string()
    }
}
