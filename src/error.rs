use std::fmt::{self, Debug, Formatter};

use crate::source::{Source, SourceRange};
use crate::token::TokenType;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TsErrorType {
    ExpectedNotFound,
    ExpectedSyntax(&'static str),
    ForLoopHeaderHasMultipleDeclarators,
    ForLoopHeaderHasNoLhs,
    ForLoopHeaderHasInvalidLhs,
    LineTerminatorAfterArrowFunctionParameters,
    LineTerminatorAfterThrow,
    LineTerminatorAfterYield,
    LineTerminatorInRegex,
    LineTerminatorInString,
    TryStatementHasNoCatchOrFinally,
    MalformedLiteralNumber,
    RequiredTokenNotFound(TokenType),
    UnexpectedEnd,
}

#[derive(Clone)]
pub struct TsError {
    position: usize,
    source: Source,
    typ: TsErrorType,
}

impl TsError {
    pub fn new(typ: TsErrorType, source: Source, position: usize) -> TsError {
        TsError {
            typ,
            source,
            position,
        }
    }

    pub fn from_loc(loc: &SourceRange, typ: TsErrorType) -> TsError {
        TsError {
            typ,
            source: loc.source.clone(),
            position: loc.start,
        }
    }

    pub fn typ(&self) -> TsErrorType {
        self.typ
    }
}

impl Debug for TsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{:?} [{}:{}]",
            self.typ,
            self.source.path().display(),
            self.position
        ))
    }
}

impl PartialEq for TsError {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Eq for TsError {}

pub type TsResult<T> = Result<T, TsError>;
