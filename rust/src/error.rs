use std::fmt::{self, Debug, Formatter};

use crate::source::SourceRange;
use crate::token::TokenType;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SyntaxErrorType {
    DuplicateVarDecl,
    ExpectedNotFound,
    ExpectedSyntax(&'static str),
    ForLoopHeaderHasInvalidLhs,
    ForLoopHeaderHasMultipleDeclarators,
    ForLoopHeaderHasNoLhs,
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
    position: usize,
    typ: SyntaxErrorType,
}

impl SyntaxError {
    pub fn new(typ: SyntaxErrorType, position: usize) -> SyntaxError {
        SyntaxError { typ, position }
    }

    pub fn from_loc(loc: &SourceRange, typ: SyntaxErrorType) -> SyntaxError {
        SyntaxError {
            typ,
            position: loc.start,
        }
    }

    pub fn typ(&self) -> SyntaxErrorType {
        self.typ
    }
}

impl Debug for SyntaxError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?} [{}]", self.typ, self.position))
    }
}

impl PartialEq for SyntaxError {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Eq for SyntaxError {}

pub type TsResult<T> = Result<T, SyntaxError>;
