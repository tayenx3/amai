pub struct Arena {
    inner: Vec<u8>,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
        }
    }

    pub fn alloc(&mut self, size: usize, align: usize) -> usize {
        let current = self.inner.len();
        let aligned_offset = (current + align - 1) & !(align - 1); // wtf is this formula

        let to_be_allocated_len = aligned_offset + size;
        self.inner.resize(to_be_allocated_len, 0);

        aligned_offset
    }

    pub fn write(&mut self, addr: usize, data: &[u8]) {
        self.inner[addr..(addr + data.len())].copy_from_slice(data);
    }

    pub fn fetch(&self, addr: usize, size: usize) -> &[u8] {
        &self.inner[addr..(addr + size)]
    }
}