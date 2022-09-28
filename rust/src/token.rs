use std::collections::HashSet;

use crate::error::{SyntaxError, SyntaxErrorType};
use crate::source::SourceRange;
use lazy_static::lazy_static;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum TokenType {
    // Used to represent a type that should never be seen in actual code. Similar to 0xFF from UTF-8
    // bytes perspective. Often used to represent an omitted value without having to use `Option`.
    _Dummy,

    Ampersand,
    AmpersandAmpersand,
    AmpersandAmpersandEquals,
    AmpersandEquals,
    Asterisk,
    AsteriskAsterisk,
    AsteriskAsteriskEquals,
    AsteriskEquals,
    Bar,
    BarBar,
    BarBarEquals,
    BarEquals,
    BraceClose,
    BraceOpen,
    BracketClose,
    BracketOpen,
    Caret,
    CaretEquals,
    ChevronLeft,
    ChevronLeftChevronLeft,
    ChevronLeftChevronLeftEquals,
    ChevronLeftEquals,
    ChevronRight,
    ChevronRightChevronRight,
    ChevronRightChevronRightChevronRight,
    ChevronRightChevronRightChevronRightEquals,
    ChevronRightChevronRightEquals,
    ChevronRightEquals,
    Colon,
    Comma,
    CommentMultiple,
    CommentSingle,
    Dot,
    DotDotDot,
    EOF,
    Equals,
    EqualsChevronRight,
    EqualsEquals,
    EqualsEqualsEquals,
    Exclamation,
    ExclamationEquals,
    ExclamationEqualsEquals,
    Hyphen,
    HyphenEquals,
    HyphenHyphen,
    Identifier,
    KeywordAs,
    KeywordAsync,
    KeywordAwait,
    KeywordBreak,
    KeywordCase,
    KeywordCatch,
    KeywordClass,
    KeywordConst,
    KeywordConstructor,
    KeywordContinue,
    KeywordDebugger,
    KeywordDefault,
    KeywordDelete,
    KeywordDo,
    KeywordElse,
    KeywordExport,
    KeywordExtends,
    KeywordFinally,
    KeywordFor,
    KeywordFrom,
    KeywordFunction,
    KeywordGet,
    KeywordIf,
    KeywordImport,
    KeywordIn,
    KeywordInstanceof,
    KeywordLet,
    KeywordNew,
    KeywordOf,
    KeywordReturn,
    KeywordSet,
    KeywordStatic,
    KeywordSuper,
    KeywordSwitch,
    KeywordThis,
    KeywordThrow,
    KeywordTry,
    KeywordTypeof,
    KeywordVar,
    KeywordVoid,
    KeywordWhile,
    KeywordWith,
    KeywordYield,
    LiteralFalse,
    LiteralNull,
    LiteralNumber,
    // LiteralNumber* are only used for lexing
    LiteralNumberHex,
    LiteralNumberBin,
    LiteralNumberOct,
    LiteralRegex,
    LiteralString,
    LiteralTemplatePartString,
    LiteralTemplatePartStringEnd,
    LiteralTrue,
    LiteralUndefined,
    ParenthesisClose,
    ParenthesisOpen,
    Percent,
    PercentEquals,
    Plus,
    PlusEquals,
    PlusPlus,
    PrivateMember,
    Question,
    QuestionDot,
    QuestionDotBracketOpen,
    QuestionDotParenthesisOpen,
    QuestionQuestion,
    QuestionQuestionEquals,
    Semicolon,
    Slash,
    SlashEquals,
    Tilde,
}

lazy_static! {
  // These can be used as parameter and variable names.
  pub static ref UNRESERVED_KEYWORDS: HashSet<TokenType> = {
    let mut set = HashSet::<TokenType>::new();
    set.insert(TokenType::KeywordAs);
    set.insert(TokenType::KeywordAsync);
    set.insert(TokenType::KeywordConstructor);
    set.insert(TokenType::KeywordFrom);
    set.insert(TokenType::KeywordGet);
    set.insert(TokenType::KeywordLet);
    set.insert(TokenType::KeywordOf);
    set.insert(TokenType::KeywordSet);
    set.insert(TokenType::KeywordStatic);
    set.insert(TokenType::LiteralUndefined);
    set
  };
}

#[derive(Clone, Debug)]
pub struct Token {
    loc: SourceRange,
    // Whether one or more whitespace characters appear immediately before this token, and at least
    // one of those whitespace characters is a line terminator.
    preceded_by_line_terminator: bool,
    typ: TokenType,
}

impl Token {
    pub fn new(loc: SourceRange, typ: TokenType, preceded_by_line_terminator: bool) -> Token {
        Token {
            loc,
            typ,
            preceded_by_line_terminator,
        }
    }

    pub fn typ(&self) -> TokenType {
        self.typ
    }

    pub fn loc(&self) -> &SourceRange {
        &self.loc
    }

    pub fn loc_take(self) -> SourceRange {
        self.loc
    }

    pub fn error(&self, typ: SyntaxErrorType) -> SyntaxError {
        SyntaxError::from_loc(&self.loc, typ, Some(self.typ.clone()))
    }

    pub fn preceded_by_line_terminator(&self) -> bool {
        self.preceded_by_line_terminator
    }
}
