use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::operator::{Operator, OperatorName, OPERATORS};
use crate::token::TokenType;

lazy_static! {
    pub static ref MULTARY_OPERATOR_MAPPING: HashMap<TokenType, &'static Operator> = {
        let mut map = HashMap::<TokenType, &'static Operator>::new();
        map.insert(TokenType::Plus, &OPERATORS[&OperatorName::Addition]);
        map.insert(TokenType::Equals, &OPERATORS[&OperatorName::Assignment]);
        map.insert(TokenType::PlusEquals, &OPERATORS[&OperatorName::AssignmentAddition]);
        map.insert(TokenType::AmpersandEquals, &OPERATORS[&OperatorName::AssignmentBitwiseAnd]);
        map.insert(TokenType::ChevronLeftChevronLeftEquals, &OPERATORS[&OperatorName::AssignmentBitwiseLeftShift]);
        map.insert(TokenType::BarEquals, &OPERATORS[&OperatorName::AssignmentBitwiseOr]);
        map.insert(TokenType::ChevronRightChevronRightEquals, &OPERATORS[&OperatorName::AssignmentBitwiseRightShift]);
        map.insert(TokenType::ChevronRightChevronRightChevronRightEquals, &OPERATORS[&OperatorName::AssignmentBitwiseUnsignedRightShift]);
        map.insert(TokenType::CaretEquals, &OPERATORS[&OperatorName::AssignmentBitwiseXor]);
        map.insert(TokenType::SlashEquals, &OPERATORS[&OperatorName::AssignmentDivision]);
        map.insert(TokenType::AsteriskAsteriskEquals, &OPERATORS[&OperatorName::AssignmentExponentiation]);
        map.insert(TokenType::AmpersandAmpersandEquals, &OPERATORS[&OperatorName::AssignmentLogicalAnd]);
        map.insert(TokenType::BarBarEquals, &OPERATORS[&OperatorName::AssignmentLogicalOr]);
        map.insert(TokenType::AsteriskEquals, &OPERATORS[&OperatorName::AssignmentMultiplication]);
        map.insert(TokenType::QuestionQuestionEquals, &OPERATORS[&OperatorName::AssignmentNullishCoalescing]);
        map.insert(TokenType::PercentEquals, &OPERATORS[&OperatorName::AssignmentRemainder]);
        map.insert(TokenType::HyphenEquals, &OPERATORS[&OperatorName::AssignmentSubtraction]);
        map.insert(TokenType::Ampersand, &OPERATORS[&OperatorName::BitwiseAnd]);
        map.insert(TokenType::ChevronLeftChevronLeft, &OPERATORS[&OperatorName::BitwiseLeftShift]);
        map.insert(TokenType::Bar, &OPERATORS[&OperatorName::BitwiseOr]);
        map.insert(TokenType::ChevronRightChevronRight, &OPERATORS[&OperatorName::BitwiseRightShift]);
        map.insert(TokenType::ChevronRightChevronRightChevronRight, &OPERATORS[&OperatorName::BitwiseUnsignedRightShift]);
        map.insert(TokenType::Caret, &OPERATORS[&OperatorName::BitwiseXor]);
        map.insert(TokenType::ParenthesisOpen, &OPERATORS[&OperatorName::Call]);
        map.insert(TokenType::Comma, &OPERATORS[&OperatorName::Comma]);
        map.insert(TokenType::BracketOpen, &OPERATORS[&OperatorName::ComputedMemberAccess]);
        map.insert(TokenType::Question, &OPERATORS[&OperatorName::Conditional]);
        map.insert(TokenType::Slash, &OPERATORS[&OperatorName::Division]);
        map.insert(TokenType::EqualsEquals, &OPERATORS[&OperatorName::Equality]);
        map.insert(TokenType::AsteriskAsterisk, &OPERATORS[&OperatorName::Exponentiation]);
        map.insert(TokenType::ChevronRight, &OPERATORS[&OperatorName::GreaterThan]);
        map.insert(TokenType::ChevronRightEquals, &OPERATORS[&OperatorName::GreaterThanOrEqual]);
        map.insert(TokenType::KeywordIn, &OPERATORS[&OperatorName::In]);
        map.insert(TokenType::ExclamationEquals, &OPERATORS[&OperatorName::Inequality]);
        map.insert(TokenType::KeywordInstanceof, &OPERATORS[&OperatorName::Instanceof]);
        map.insert(TokenType::ChevronLeft, &OPERATORS[&OperatorName::LessThan]);
        map.insert(TokenType::ChevronLeftEquals, &OPERATORS[&OperatorName::LessThanOrEqual]);
        map.insert(TokenType::AmpersandAmpersand, &OPERATORS[&OperatorName::LogicalAnd]);
        map.insert(TokenType::BarBar, &OPERATORS[&OperatorName::LogicalOr]);
        map.insert(TokenType::Dot, &OPERATORS[&OperatorName::MemberAccess]);
        map.insert(TokenType::Asterisk, &OPERATORS[&OperatorName::Multiplication]);
        map.insert(TokenType::QuestionQuestion, &OPERATORS[&OperatorName::NullishCoalescing]);
        map.insert(TokenType::QuestionDot, &OPERATORS[&OperatorName::OptionalChainingMemberAccess]);
        map.insert(TokenType::QuestionDotBracketOpen, &OPERATORS[&OperatorName::OptionalChainingComputedMemberAccess]);
        map.insert(TokenType::QuestionDotParenthesisOpen, &OPERATORS[&OperatorName::OptionalChainingCall]);
        map.insert(TokenType::Percent, &OPERATORS[&OperatorName::Remainder]);
        map.insert(TokenType::EqualsEqualsEquals, &OPERATORS[&OperatorName::StrictEquality]);
        map.insert(TokenType::ExclamationEqualsEquals, &OPERATORS[&OperatorName::StrictInequality]);
        map.insert(TokenType::Hyphen, &OPERATORS[&OperatorName::Subtraction]);
        map.insert(TokenType::KeywordTypeof, &OPERATORS[&OperatorName::Typeof]);
        map
    };

    pub static ref UNARY_OPERATOR_MAPPING: HashMap<TokenType, &'static Operator> = {
        let mut map = HashMap::<TokenType, &'static Operator>::new();
        // Postfix{Increment,Decrement} and YieldDelegated omitted and handled manually.
        map.insert(TokenType::KeywordAwait, &OPERATORS[&OperatorName::Await]);
        map.insert(TokenType::Tilde, &OPERATORS[&OperatorName::BitwiseNot]);
        map.insert(TokenType::KeywordDelete, &OPERATORS[&OperatorName::Delete]);
        map.insert(TokenType::Exclamation, &OPERATORS[&OperatorName::LogicalNot]);
        map.insert(TokenType::KeywordNew, &OPERATORS[&OperatorName::New]);
        map.insert(TokenType::HyphenHyphen, &OPERATORS[&OperatorName::PrefixDecrement]);
        map.insert(TokenType::PlusPlus, &OPERATORS[&OperatorName::PrefixIncrement]);
        map.insert(TokenType::Hyphen, &OPERATORS[&OperatorName::UnaryNegation]);
        map.insert(TokenType::Plus, &OPERATORS[&OperatorName::UnaryPlus]);
        map.insert(TokenType::KeywordTypeof, &OPERATORS[&OperatorName::Typeof]);
        map.insert(TokenType::KeywordVoid, &OPERATORS[&OperatorName::Void]);
        map.insert(TokenType::KeywordYield, &OPERATORS[&OperatorName::Yield]);
        map
    };
}
