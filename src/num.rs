use std::hash::{Hash, Hasher};
use std::mem;

// This provides Eq for f64.
#[derive(Clone, Debug)]
pub struct JsNumber(pub f64);

impl PartialEq for JsNumber {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() {
            return other.0.is_nan();
        };
        self.0.eq(&other.0)
    }
}

impl Eq for JsNumber {}

impl Hash for JsNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if !self.0.is_nan() {
            unsafe { mem::transmute::<f64, u64>(self.0) }.hash(state);
        };
    }
}
