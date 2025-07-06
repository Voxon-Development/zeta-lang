use ir::bump::{AtomicBump, Bump};

pub struct ByteWriter {
    pub buffer: Vec<u8, AtomicBump>,
}

impl ByteWriter {
    #[inline]
    pub fn new() -> Self {
        Self { buffer: Vec::new_in(AtomicBump::new()) }
    }

    #[inline]
    pub fn extend(&mut self, other: Vec<u8, AtomicBump>) {
        self.buffer.extend(other);
    }

    #[inline]
    pub fn write_u8(&mut self, value: u8) {
        self.buffer.push(value);
    }

    #[inline]
    pub fn write_u16(&mut self, value: u16) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    #[inline]
    pub fn write_u32(&mut self, value: u32) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    #[inline]
    pub fn write_u64(&mut self, value: u64) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    #[inline]
    pub fn write_i64(&mut self, value: i64) {
        self.buffer.extend_from_slice(&value.to_le_bytes());
    }

    #[inline]
    pub fn write_string(&mut self, value: &str) {
        self.write_u16(value.len() as u16); // allows up to 65535-byte names
        self.buffer.extend_from_slice(value.as_bytes());
    }

    #[inline]
    pub fn into_bytes(self) -> Vec<u8, AtomicBump> {
        self.buffer
    }
}