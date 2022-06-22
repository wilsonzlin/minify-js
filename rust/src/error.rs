use std::cmp::{max, min};
use std::fmt::{self, Debug, Formatter};
use std::str::from_utf8_unchecked;

use crate::source::{Source, SourceRange};
use crate::token::TokenType;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SyntaxErrorType {
    ExpectedNotFound,
    ExpectedSyntax(&'static str),
    ForLoopHeaderHasInvalidLhs,
    ForLoopHeaderHasMultipleDeclarators,
    ForLoopHeaderHasNoLhs,
    InvalidAssigmentTarget,
    LineTerminatorAfterArrowFunctionParameters,
    LineTerminatorAfterThrow,
    LineTerminatorAfterYield,
    LineTerminatorInRegex,
    LineTerminatorInString,
    MalformedLiteralNumber,
    RequiredTokenNotFound(TokenType),
    TryStatementHasNoCatchOrFinally,
    UnexpectedEnd,
}

#[derive(Clone)]
pub struct SyntaxError {
    source: Source,
    position: usize,
    typ: SyntaxErrorType,
    actual_token: Option<TokenType>,
}

impl SyntaxError {
    pub fn new(
        typ: SyntaxErrorType,
        source: Source,
        position: usize,
        actual_token: Option<TokenType>,
    ) -> SyntaxError {
        SyntaxError {
            typ,
            source,
            position,
            actual_token,
        }
    }

    pub fn from_loc(
        loc: &SourceRange,
        typ: SyntaxErrorType,
        actual_token: Option<TokenType>,
    ) -> SyntaxError {
        SyntaxError {
            source: loc.source.clone(),
            typ,
            position: loc.start,
            actual_token,
        }
    }

    pub fn typ(&self) -> SyntaxErrorType {
        self.typ
    }
}

impl Debug for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:?} [position={} token={:?}] around ```{}```",
            self.typ,
            self.position,
            self.actual_token,
            unsafe {
                from_utf8_unchecked(
                    &self.source.code()[max(0, self.position as isize - 40) as usize
                        ..min(
                            self.source.code().len() as isize,
                            self.position as isize + 40,
                        ) as usize],
                )
            }
        ))
    }
}

impl PartialEq for SyntaxError {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Eq for SyntaxError {}

pub type SyntaxResult<T> = Result<T, SyntaxError>;
