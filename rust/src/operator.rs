use crate::operator::Arity::*;
use crate::operator::Associativity::*;
use crate::operator::OperatorName::*;
use lazy_static::lazy_static;
#[cfg(test)]
use serde::Serialize;
use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum OperatorName {
    Addition,
    Assignment,
    AssignmentAddition,
    AssignmentBitwiseAnd,
    AssignmentBitwiseLeftShift,
    AssignmentBitwiseOr,
    AssignmentBitwiseRightShift,
    AssignmentBitwiseUnsignedRightShift,
    AssignmentBitwiseXor,
    AssignmentDivision,
    AssignmentExponentiation,
    AssignmentLogicalAnd,
    AssignmentLogicalOr,
    AssignmentMultiplication,
    AssignmentNullishCoalescing,
    AssignmentRemainder,
    AssignmentSubtraction,
    Await,
    BitwiseAnd,
    BitwiseLeftShift,
    BitwiseNot,
    BitwiseOr,
    BitwiseRightShift,
    BitwiseUnsignedRightShift,
    BitwiseXor,
    Call,
    Comma,
    ComputedMemberAccess,
    Conditional,
    // Only used during parsing.
    ConditionalAlternate,
    Delete,
    Division,
    Equality,
    Exponentiation,
    GreaterThan,
    GreaterThanOrEqual,
    In,
    Inequality,
    Instanceof,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalNot,
    LogicalOr,
    MemberAccess,
    Multiplication,
    New,
    NullishCoalescing,
    OptionalChainingMemberAccess,
    OptionalChainingComputedMemberAccess,
    OptionalChainingCall,
    PostfixDecrement,
    PostfixIncrement,
    PrefixDecrement,
    PrefixIncrement,
    Remainder,
    StrictEquality,
    StrictInequality,
    Subtraction,
    Typeof,
    UnaryNegation,
    UnaryPlus,
    Void,
    Yield,
    YieldDelegated,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Arity {
    Unary,
    Binary,
    Ternary,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Associativity {
    Left,
    Right,
}

pub struct Operator {
    pub name: OperatorName,
    pub arity: Arity,
    pub associativity: Associativity,
    pub precedence: u8,
}

const PRECEDENCE_LEVELS: &'static [&'static [(OperatorName, Arity, Associativity)]] = &[
    &[
        (MemberAccess, Binary, Left),
        (ComputedMemberAccess, Binary, Left),
        (Call, Binary, Left),
        (New, Unary, Right),
        (OptionalChainingMemberAccess, Binary, Left),
        (OptionalChainingComputedMemberAccess, Binary, Left),
        (OptionalChainingCall, Binary, Left),
    ],
    &[
        (PostfixIncrement, Unary, Left),
        (PostfixDecrement, Unary, Left),
    ],
    &[
        (LogicalNot, Unary, Right),
        (BitwiseNot, Unary, Right),
        (UnaryPlus, Unary, Right),
        (UnaryNegation, Unary, Right),
        (PrefixIncrement, Unary, Right),
        (PrefixDecrement, Unary, Right),
        (Typeof, Unary, Right),
        (Void, Unary, Right),
        (Delete, Unary, Right),
        (Await, Unary, Right),
    ],
    &[(Exponentiation, Binary, Right)],
    &[
        (Multiplication, Binary, Left),
        (Division, Binary, Left),
        (Remainder, Binary, Left),
    ],
    &[(Addition, Binary, Left), (Subtraction, Binary, Left)],
    &[
        (BitwiseLeftShift, Binary, Left),
        (BitwiseRightShift, Binary, Left),
        (BitwiseUnsignedRightShift, Binary, Left),
    ],
    &[
        (LessThan, Binary, Left),
        (LessThanOrEqual, Binary, Left),
        (GreaterThan, Binary, Left),
        (GreaterThanOrEqual, Binary, Left),
        (In, Binary, Left),
        (Instanceof, Binary, Left),
    ],
    &[
        (Equality, Binary, Left),
        (Inequality, Binary, Left),
        (StrictEquality, Binary, Left),
        (StrictInequality, Binary, Left),
    ],
    &[(BitwiseAnd, Binary, Left)],
    &[(BitwiseXor, Binary, Left)],
    &[(BitwiseOr, Binary, Left)],
    &[(LogicalAnd, Binary, Left)],
    &[(LogicalOr, Binary, Left), (NullishCoalescing, Binary, Left)],
    &[(Conditional, Ternary, Right)],
    &[
        (Assignment, Binary, Right),
        (AssignmentAddition, Binary, Right),
        (AssignmentBitwiseAnd, Binary, Right),
        (AssignmentBitwiseLeftShift, Binary, Right),
        (AssignmentBitwiseOr, Binary, Right),
        (AssignmentBitwiseRightShift, Binary, Right),
        (AssignmentBitwiseUnsignedRightShift, Binary, Right),
        (AssignmentBitwiseXor, Binary, Right),
        (AssignmentDivision, Binary, Right),
        (AssignmentExponentiation, Binary, Right),
        (AssignmentLogicalAnd, Binary, Right),
        (AssignmentLogicalOr, Binary, Right),
        (AssignmentMultiplication, Binary, Right),
        (AssignmentNullishCoalescing, Binary, Right),
        (AssignmentRemainder, Binary, Right),
        (AssignmentSubtraction, Binary, Right),
        (Yield, Unary, Right),
        (YieldDelegated, Unary, Right),
    ],
    // Given `a, b = true ? c : d = e, f`, the evaluation is `a, (b = (true ? c : (d = e))), e`.
    &[(ConditionalAlternate, Ternary, Right)],
    &[(Comma, Binary, Left)],
];

lazy_static! {
    pub static ref OPERATORS: HashMap<OperatorName, Operator> = {
        let mut map = HashMap::<OperatorName, Operator>::new();
        for (i, ops) in PRECEDENCE_LEVELS.iter().enumerate() {
            let precedence = (PRECEDENCE_LEVELS.len() - i) as u8;
            for &(name, arity, associativity) in ops.iter() {
                map.insert(
                    name,
                    Operator {
                        name,
                        arity,
                        associativity,
                        precedence,
                    },
                );
            }
        }
        map
    };
}
