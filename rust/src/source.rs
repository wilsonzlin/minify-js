use std::cmp::{max, min, Eq};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign};
use std::rc::Rc;

/// A string backed by a source. Treated as a string, so contents rather than position is considered
/// the value. For example, two SourceRange values are equal if their contents equal, even if they
/// are from different files or positions in the same file.
#[derive(Clone)]
pub struct SourceRange {
    pub source: Source,
    pub start: usize,
    pub end: usize,
}

impl SourceRange {
    pub fn anonymous<T: Into<Vec<u8>>>(code: T) -> SourceRange {
        let code = code.into();
        let end = code.len();
        SourceRange {
            source: Source::new(code),
            start: 0,
            end,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.start >= self.source.code().len()
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.source.code()[self.start..self.end]
    }

    pub fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_slice()) }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn extend(&mut self, other: &SourceRange) {
        self.start = min(self.start, other.start);
        self.end = max(self.end, other.end);
    }
}

impl Add for &SourceRange {
    type Output = SourceRange;

    fn add(self, rhs: Self) -> Self::Output {
        SourceRange {
            source: self.source.clone(),
            start: min(self.start, rhs.start),
            end: max(self.end, rhs.end),
        }
    }
}

impl AddAssign for SourceRange {
    fn add_assign(&mut self, rhs: Self) {
        self.extend(&rhs);
    }
}

impl Debug for SourceRange {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_eof() {
            Ok(())
        } else {
            f.write_str(&format!("`{}`[{}:{}]", self.as_str(), self.start, self.end))
        }
    }
}

impl Eq for SourceRange {}

impl Hash for SourceRange {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if !self.is_eof() {
            self.as_slice().hash(state);
        };
    }
}

impl PartialEq for SourceRange {
    fn eq(&self, other: &Self) -> bool {
        if self.is_eof() {
            other.is_eof()
        } else {
            self.as_slice() == other.as_slice()
        }
    }
}

impl PartialEq<str> for SourceRange {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

struct SourceData {
    code: Vec<u8>,
}

#[derive(Clone)]
pub struct Source(Rc<SourceData>);

impl Source {
    pub fn new(code: Vec<u8>) -> Source {
        Source(Rc::new(SourceData { code }))
    }

    pub fn code(&self) -> &[u8] {
        &self.0.code
    }
}
