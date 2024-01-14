pub(crate) struct Counter(u32);

impl Counter {
  pub fn new(init: u32) -> Self {
    Self(init)
  }

  pub fn bump(&mut self) -> u32 {
    let v = self.0;
    self.0 += 1;
    v
  }
}
