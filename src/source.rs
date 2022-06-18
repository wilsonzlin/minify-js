use std::cmp::{max, min, Eq};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, AddAssign};
use std::path::{Path, PathBuf};
use std::sync::Arc;

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
        debug_assert_eq!(self.source.path(), other.source.path());
        self.start = min(self.start, other.start);
        self.end = max(self.end, other.end);
    }
}

impl Add for &SourceRange {
    type Output = SourceRange;

    fn add(self, rhs: Self) -> Self::Output {
        debug_assert_eq!(self.source.path(), rhs.source.path());
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
            f.write_str(self.as_str())
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
    path: PathBuf,
    code: Vec<u8>,
}

impl Debug for SourceData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.path.fmt(f)
    }
}

#[derive(Clone)]
pub struct Source(Arc<SourceData>);

impl Debug for Source {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Source {
    pub fn new(name: PathBuf, code: Vec<u8>) -> Source {
        Source(Arc::new(SourceData { path: name, code }))
    }

    pub fn path(&self) -> &Path {
        &self.0.path
    }

    pub fn code(&self) -> &[u8] {
        &self.0.code
    }
}
