use std::fmt::{self, Debug, Formatter};

use crate::source::SourceRange;
use crate::token::TokenType;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SyntaxErrorType {
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
    actual_token: Option<TokenType>,
}

impl SyntaxError {
    pub fn new(
        typ: SyntaxErrorType,
        position: usize,
        actual_token: Option<TokenType>,
    ) -> SyntaxError {
        SyntaxError {
            typ,
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
            "{:?} [{} {:?}]",
            self.typ, self.position, self.actual_token
        ))
    }
}

impl PartialEq for SyntaxError {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Eq for SyntaxError {}

pub type TsResult<T> = Result<T, SyntaxError>;
