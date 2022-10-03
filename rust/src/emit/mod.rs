use parse_js::ast::{
    ArrayElement, ClassMember, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ExportNames,
    ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, LiteralTemplatePart, NodeId, NodeMap,
    ObjectMemberType, Syntax, VarDeclMode,
};
use parse_js::operator::{OperatorName, OPERATORS};
use lazy_static::lazy_static;
use std::io::Write;
use std::{collections::HashMap, io};

#[cfg(test)]
mod tests;

lazy_static! {
  pub static ref BINARY_OPERATOR_SYNTAX: HashMap<OperatorName, &'static str> = {
      let mut map = HashMap::<OperatorName, &'static str>::new();
      // Excluded: Call, Conditional.
      map.insert(OperatorName::Addition, "+");
      map.insert(OperatorName::Assignment, "=");
      map.insert(OperatorName::AssignmentAddition, "+=");
      map.insert(OperatorName::AssignmentBitwiseAnd, "&=");
      map.insert(OperatorName::AssignmentBitwiseLeftShift, "<<=");
      map.insert(OperatorName::AssignmentBitwiseOr, "|=");
      map.insert(OperatorName::AssignmentBitwiseRightShift, ">>=");
      map.insert(OperatorName::AssignmentBitwiseUnsignedRightShift, ">>>=");
      map.insert(OperatorName::AssignmentBitwiseXor, "^=");
      map.insert(OperatorName::AssignmentDivision, "/=");
      map.insert(OperatorName::AssignmentExponentiation, "**=");
      map.insert(OperatorName::AssignmentLogicalAnd, "&&=");
      map.insert(OperatorName::AssignmentLogicalOr, "||=");
      map.insert(OperatorName::AssignmentMultiplication, "*=");
      map.insert(OperatorName::AssignmentNullishCoalescing, "??=");
      map.insert(OperatorName::AssignmentRemainder, "%=");
      map.insert(OperatorName::AssignmentSubtraction, "-=");
      map.insert(OperatorName::BitwiseAnd, "&");
      map.insert(OperatorName::BitwiseLeftShift, "<<");
      map.insert(OperatorName::BitwiseOr, "|");
      map.insert(OperatorName::BitwiseRightShift, ">>");
      map.insert(OperatorName::BitwiseUnsignedRightShift, ">>>");
      map.insert(OperatorName::BitwiseXor, "^");
      map.insert(OperatorName::Comma, ",");
      map.insert(OperatorName::Division, "/");
      map.insert(OperatorName::Equality, "==");
      map.insert(OperatorName::Exponentiation, "**");
      map.insert(OperatorName::GreaterThan, ">");
      map.insert(OperatorName::GreaterThanOrEqual, ">=");
      map.insert(OperatorName::In, " in ");
      map.insert(OperatorName::Inequality, "!=");
      map.insert(OperatorName::Instanceof, " instanceof ");
      map.insert(OperatorName::LessThan, "<");
      map.insert(OperatorName::LessThanOrEqual, "<=");
      map.insert(OperatorName::LogicalAnd, "&&");
      map.insert(OperatorName::LogicalOr, "||");
      map.insert(OperatorName::MemberAccess, ".");
      map.insert(OperatorName::Multiplication, "*");
      map.insert(OperatorName::NullishCoalescing, "??");
      map.insert(OperatorName::OptionalChainingMemberAccess, "?.");
      map.insert(OperatorName::OptionalChainingComputedMemberAccess, "?.[");
      map.insert(OperatorName::OptionalChainingCall, "?.(");
      map.insert(OperatorName::Remainder, "%");
      map.insert(OperatorName::StrictEquality, "===");
      map.insert(OperatorName::StrictInequality, "!==");
      map.insert(OperatorName::Subtraction, "-");
      map.insert(OperatorName::Typeof, " typeof ");
      map
  };

  pub static ref UNARY_OPERATOR_SYNTAX: HashMap<OperatorName, &'static str> = {
      let mut map = HashMap::<OperatorName, &'static str>::new();
      // Excluded: Postfix{Increment,Decrement}.
      map.insert(OperatorName::Await, "await ");
      map.insert(OperatorName::BitwiseNot, "~");
      map.insert(OperatorName::Delete, "delete ");
      map.insert(OperatorName::LogicalNot, "!");
      map.insert(OperatorName::New, "new ");
      map.insert(OperatorName::PrefixDecrement, "--");
      map.insert(OperatorName::PrefixIncrement, "++");
      map.insert(OperatorName::Typeof, "typeof ");
      map.insert(OperatorName::UnaryNegation, "-");
      map.insert(OperatorName::UnaryPlus, "+");
      map.insert(OperatorName::Void, "void ");
      map.insert(OperatorName::Yield, "yield ");
      map.insert(OperatorName::YieldDelegated, "yield*");
      map
  };
}

// Returns whether or not the value is a property.
fn emit_class_or_object_member<T: Write>(
    out: &mut T,
    map: &NodeMap,
    key: &ClassOrObjectMemberKey,
    value: &ClassOrObjectMemberValue,
    value_delimiter: &[u8],
) -> io::Result<bool> {
    let is_computed_key = match key {
        ClassOrObjectMemberKey::Computed(_) => true,
        _ => false,
    };
    match value {
        ClassOrObjectMemberValue::Getter { .. } => {
            out.write_all(b"get")?;
            if !is_computed_key {
                out.write_all(b" ")?;
            };
        }
        ClassOrObjectMemberValue::Setter { .. } => {
            out.write_all(b"set")?;
            if !is_computed_key {
                out.write_all(b" ")?;
            };
        }
        ClassOrObjectMemberValue::Method {
            is_async,
            generator,
            ..
        } => {
            if *is_async {
                out.write_all(b"async")?;
            }
            if *generator {
                out.write_all(b"*")?;
            } else if *is_async {
                out.write_all(b" ")?;
            }
        }
        _ => {}
    };
    match key {
        ClassOrObjectMemberKey::Direct(name) => {
            out.write_all(name.as_slice())?;
        }
        ClassOrObjectMemberKey::Computed(expr) => {
            out.write_all(b"[")?;
            emit_js(out, map, *expr)?;
            out.write_all(b"]")?;
        }
    };
    match value {
        ClassOrObjectMemberValue::Getter { body } => {
            out.write_all(b"()")?;
            emit_js(out, map, *body)?;
        }
        ClassOrObjectMemberValue::Method {
            signature, body, ..
        } => {
            out.write_all(b"(")?;
            emit_js(out, map, *signature)?;
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
        }
        ClassOrObjectMemberValue::Property { initializer } => {
            if let Some(v) = initializer {
                out.write_all(value_delimiter)?;
                emit_js(out, map, *v)?;
            };
        }
        ClassOrObjectMemberValue::Setter { body, parameter } => {
            out.write_all(b"(")?;
            emit_js(out, map, *parameter)?;
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
        }
    };

    Ok(match value {
        ClassOrObjectMemberValue::Property { .. } => true,
        _ => false,
    })
}

fn emit_class<T: Write>(
    out: &mut T,
    map: &NodeMap,
    name: Option<NodeId>,
    extends: Option<NodeId>,
    members: &Vec<ClassMember>,
) -> io::Result<()> {
    out.write_all(b"class")?;
    if let Some(n) = name {
        out.write_all(b" ")?;
        emit_js(out, map, n)?;
    }
    if let Some(s) = extends {
        out.write_all(b" extends ")?;
        emit_js(out, map, s)?;
    }
    out.write_all(b"{")?;
    let mut last_member_was_property = false;
    for (i, m) in members.iter().enumerate() {
        if i > 0 && last_member_was_property {
            out.write_all(b";")?;
        }
        if m.statik {
            out.write_all(b"static ")?;
        }
        last_member_was_property = emit_class_or_object_member(out, map, &m.key, &m.value, b"=")?;
    }
    out.write_all(b"}")?;
    Ok(())
}

fn emit_import_or_export_statement_trailer<T: Write>(
    out: &mut T,
    m: &NodeMap,
    names: Option<&ExportNames>,
    from: Option<&String>,
) -> io::Result<()> {
    match names {
        Some(ExportNames::All(alias)) => {
            out.write_all(b"*")?;
            if let Some(alias) = alias {
                out.write_all(b"as ")?;
                emit_js(out, m, *alias)?;
                if from.is_some() {
                    out.write_all(b" ")?;
                }
            };
        }
        Some(ExportNames::Specific(names)) => {
            out.write_all(b"{")?;
            for (i, e) in names.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                out.write_all(e.target.as_slice())?;
                // TODO Omit if identical to `target`.
                out.write_all(b" as ")?;
                emit_js(out, m, e.alias)?;
            }
            out.write_all(b"}")?;
        }
        None => {}
    };
    if let Some(from) = from {
        out.write_all(b"from\"")?;
        // TODO Escape?
        out.write_all(from.as_bytes())?;
        out.write_all(b"\"")?;
    };
    Ok(())
}

pub fn emit_js<T: Write>(out: &mut T, m: &NodeMap, n: NodeId) -> io::Result<()> {
    emit_js_under_operator(out, m, n, None)?;
    out.flush()
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LeafNodeType {
    EmptyStmt,
    Other,
    Block,
}

fn get_leaf_node_type(m: &NodeMap, n: NodeId) -> LeafNodeType {
    match m[n].stx() {
        Syntax::WhileStmt { body, .. } | Syntax::ForStmt { body, .. } => {
            get_leaf_node_type(m, *body)
        }
        Syntax::LabelStmt { statement, .. } => get_leaf_node_type(m, *statement),
        Syntax::IfStmt {
            consequent,
            alternate,
            ..
        } => match alternate {
            Some(n) => get_leaf_node_type(m, *n),
            None => get_leaf_node_type(m, *consequent),
        },
        Syntax::BlockStmt { .. } => LeafNodeType::Block,
        Syntax::EmptyStmt {} => LeafNodeType::EmptyStmt,
        _ => LeafNodeType::Other,
    }
}

// It's important to use this function:
// - Omit semicolons where possible.
// - Insert semicolon after last statement if its leaf is a `if`, `for`, `while`, or `with` statement with an empty statement as its body e.g. `if (x) label: for (;;) while (x)` but not `if (x) for (;;) label: while (x) {}` or `if (x) for (;;) label: while (x) return`.
fn emit_statements<T: Write>(out: &mut T, m: &NodeMap, statements: &[NodeId]) -> io::Result<()> {
    // Since we skip over some statements, the last actual statement may not be the last in the list.
    let mut last_statement: Option<NodeId> = None;
    for n in statements {
        if let Some(n) = last_statement {
            match m[n].stx() {
                Syntax::EmptyStmt {} | Syntax::FunctionDecl { .. } | Syntax::ClassDecl { .. } => {}
                _ => {
                    out.write_all(b";")?;
                }
            }
        }
        emit_js(out, m, *n)?;
        last_statement = Some(*n);
    }
    if let Some(n) = last_statement {
        if get_leaf_node_type(m, n) == LeafNodeType::EmptyStmt {
            out.write_all(b";")?;
        }
    }
    Ok(())
}

/*
For `do <stmt> while (...)` and `if <stmt> else (...)`, when does a semicolon need to be inserted after `<stmt>`?

# Requires semicolon:
- do a + b; while (a)
- do return; while (a)
- do label: return a + b; while (a)
- do continue; while (a)
- do for (;;) while (y) if (z); while (a)

# Does not require semicolon, would cause malformed syntax:
- do {} while (a)
- do if (x) {} while (a)
- do for (;;) while (y) if (z) {} while (a);
*/

fn emit_js_under_operator<T: Write>(
    out: &mut T,
    map: &NodeMap,
    node_id: NodeId,
    parent_operator_precedence: Option<u8>,
) -> io::Result<()> {
    match map[node_id].stx() {
        Syntax::EmptyStmt {} => {}
        Syntax::LiteralBooleanExpr { .. }
        | Syntax::LiteralNumberExpr { .. }
        | Syntax::LiteralRegexExpr { .. }
        | Syntax::LiteralStringExpr { .. } => {
            out.write_all(map[node_id].loc().as_slice())?;
        }
        Syntax::LiteralTemplateExpr { parts } => {
            out.write_all(b"`")?;
            for p in parts {
                match p {
                    LiteralTemplatePart::Substitution(sub) => {
                        out.write_all(b"${")?;
                        emit_js(out, map, *sub)?;
                        out.write_all(b"}")?;
                    }
                    LiteralTemplatePart::String(str) => {
                        out.write_all(str.as_slice())?;
                    }
                }
            }
            out.write_all(b"`")?;
        }
        Syntax::VarDecl { mode, declarators } => {
            out.write_all(match mode {
                VarDeclMode::Const => b"const",
                VarDeclMode::Let => b"let",
                VarDeclMode::Var => b"var",
            })?;
            out.write_all(b" ")?;
            for (i, decl) in declarators.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                emit_js(out, map, decl.pattern)?;
                if let Some(expr) = &decl.initializer {
                    out.write_all(b"=")?;
                    // This is only really done for the Comma operator, which is the only operator below Assignment.
                    let operator = &OPERATORS[&OperatorName::Assignment];
                    emit_js_under_operator(out, map, *expr, Some(operator.precedence))?;
                };
            }
        }
        Syntax::VarStmt { declaration } => {
            emit_js(out, map, *declaration)?;
        }
        Syntax::IdentifierPattern { name } => {
            out.write_all(name.as_slice())?;
        }
        Syntax::ArrayPattern { elements, rest } => {
            out.write_all(b"[")?;
            for (i, e) in elements.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                if let Some(e) = e {
                    emit_js(out, map, e.target)?;
                    if let Some(v) = &e.default_value {
                        out.write_all(b"=")?;
                        emit_js(out, map, *v)?;
                    }
                };
            }
            if let Some(r) = rest {
                if !elements.is_empty() {
                    out.write_all(b",")?;
                }
                out.write_all(b"...")?;
                emit_js(out, map, *r)?;
            };
            out.write_all(b"]")?;
        }
        Syntax::ObjectPattern { properties, rest } => {
            out.write_all(b"{")?;
            for (i, e) in properties.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                emit_js(out, map, *e)?;
            }
            if let Some(r) = rest {
                if !properties.is_empty() {
                    out.write_all(b",")?;
                }
                out.write_all(b"...")?;
                emit_js(out, map, *r)?;
            };
            out.write_all(b"}")?;
        }
        Syntax::ClassOrFunctionName { name } => {
            out.write_all(name.as_slice())?;
        }
        Syntax::FunctionSignature { parameters } => {
            for (i, p) in parameters.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                };
                emit_js(out, map, *p)?;
            }
        }
        Syntax::ClassDecl {
            name,
            extends,
            members,
        } => {
            emit_class(out, map, *name, *extends, members)?;
        }
        Syntax::FunctionDecl {
            is_async,
            generator,
            name,
            signature,
            body,
        } => {
            if *is_async {
                out.write_all(b"async ")?;
            }
            out.write_all(b"function")?;
            if *generator {
                out.write_all(b"*")?;
            } else if name.is_some() {
                out.write_all(b" ")?;
            };
            if let Some(name) = name {
                emit_js(out, map, *name)?;
            }
            out.write_all(b"(")?;
            emit_js(out, map, *signature)?;
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
        }
        Syntax::ParamDecl {
            rest,
            pattern,
            default_value,
        } => {
            if *rest {
                out.write_all(b"...")?;
            };
            emit_js(out, map, *pattern)?;
            if let Some(v) = default_value {
                out.write_all(b"=")?;
                emit_js(out, map, *v)?;
            }
        }
        Syntax::ArrowFunctionExpr {
            is_async,
            signature,
            body,
        } => {
            if *is_async {
                out.write_all(b"async")?;
            }
            let can_omit_parentheses =
                if let Syntax::FunctionSignature { parameters } = map[*signature].stx() {
                    !is_async
                        && parameters.len() == 1
                        && match map[parameters[0]].stx() {
                            Syntax::ParamDecl {
                                default_value,
                                pattern,
                                rest,
                            } => {
                                !rest
                                    && default_value.is_none()
                                    && match map[*pattern].stx() {
                                        Syntax::IdentifierPattern { .. } => true,
                                        _ => false,
                                    }
                            }
                            _ => false,
                        }
                } else {
                    false
                };
            if !can_omit_parentheses {
                out.write_all(b"(")?;
            };
            emit_js(out, map, *signature)?;
            if !can_omit_parentheses {
                out.write_all(b")")?;
            };
            out.write_all(b"=>")?;
            emit_js(out, map, *body)?;
        }
        Syntax::BinaryExpr {
            parenthesised,
            operator: operator_name,
            left,
            right,
        } => {
            let operator = &OPERATORS[operator_name];
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                // Needed to prevent an expression statement with an assignment to an object pattern from being interpreted as a block when unwrapped.
                // TODO Omit when possible.
                None if *operator_name == OperatorName::Assignment => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            };
            emit_js_under_operator(out, map, *left, Some(operator.precedence))?;
            out.write_all(
                BINARY_OPERATOR_SYNTAX
                    .get(operator_name)
                    .unwrap()
                    .as_bytes(),
            )?;
            match operator_name {
                OperatorName::Addition | OperatorName::Subtraction => {
                    // Prevent potential confict with following unary operator e.g. `a+ +b` => `a++b`.
                    // TODO Omit when possible.
                    out.write_all(b" ")?;
                }
                _ => {}
            };
            emit_js_under_operator(out, map, *right, Some(operator.precedence))?;
            if must_parenthesise {
                out.write_all(b")")?;
            };
        }
        Syntax::CallExpr {
            optional_chaining,
            parenthesised,
            callee,
            arguments,
        } => {
            let operator = &OPERATORS[&OperatorName::Call];
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                // We need to keep parentheses to prevent function expressions from being misinterpreted as a function declaration, which cannot be part of an expression e.g. IIFE.
                // TODO Omit parentheses if possible.
                None => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            }
            emit_js_under_operator(out, map, *callee, Some(operator.precedence))?;
            if *optional_chaining {
                out.write_all(b"?.")?;
            }
            out.write_all(b"(")?;
            for (i, a) in arguments.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                emit_js(out, map, *a)?;
            }
            out.write_all(b")")?;
            // TODO Omit parentheses if possible.
            if must_parenthesise {
                out.write_all(b")")?;
            }
        }
        Syntax::ConditionalExpr {
            parenthesised,
            test,
            consequent,
            alternate,
        } => {
            let operator = &OPERATORS[&OperatorName::Conditional];
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            };
            emit_js_under_operator(out, map, *test, Some(operator.precedence))?;
            out.write_all(b"?")?;
            emit_js_under_operator(out, map, *consequent, Some(operator.precedence))?;
            out.write_all(b":")?;
            emit_js_under_operator(out, map, *alternate, Some(operator.precedence))?;
            if must_parenthesise {
                out.write_all(b")")?;
            };
        }
        Syntax::FunctionExpr {
            parenthesised,
            is_async,
            generator,
            name,
            signature,
            body,
        } => {
            // We need to keep parentheses to prevent function expressions from being misinterpreted as a function declaration, which cannot be part of an expression e.g. IIFE.
            // TODO Omit parentheses if possible.
            if *parenthesised {
                out.write_all(b"(")?;
            }
            if *is_async {
                out.write_all(b"async ")?;
            }
            out.write_all(b"function")?;
            if *generator {
                out.write_all(b"*")?;
            };
            if let Some(name) = name {
                if !generator {
                    out.write_all(b" ")?;
                };
                emit_js(out, map, *name)?;
            };
            out.write_all(b"(")?;
            emit_js(out, map, *signature)?;
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
            // TODO Omit parentheses if possible.
            if *parenthesised {
                out.write_all(b")")?;
            }
        }
        Syntax::IdentifierExpr { name } => {
            out.write_all(name.as_slice())?;
        }
        Syntax::ImportExpr { module } => {
            out.write_all(b"import(")?;
            emit_js(out, map, *module)?;
            out.write_all(b")")?;
        }
        Syntax::LiteralArrayExpr { elements } => {
            out.write_all(b"[")?;
            for (i, e) in elements.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                };
                match e {
                    ArrayElement::Single(expr) => {
                        emit_js(out, map, *expr)?;
                    }
                    ArrayElement::Rest(expr) => {
                        out.write_all(b"...")?;
                        emit_js(out, map, *expr)?;
                    }
                    ArrayElement::Empty => {}
                };
            }
            out.write_all(b"]")?;
        }
        Syntax::LiteralObjectExpr { members } => {
            out.write_all(b"{")?;
            for (i, e) in members.iter().enumerate() {
                if i > 0 {
                    out.write_all(b",")?;
                }
                emit_js(out, map, *e)?;
            }
            out.write_all(b"}")?;
        }
        Syntax::LiteralNull {} => {
            out.write_all(b"null")?;
        }
        Syntax::LiteralUndefined {} => {
            out.write_all(b"undefined")?;
        }
        Syntax::UnaryExpr {
            parenthesised,
            operator: operator_name,
            argument,
        } => {
            let operator = OPERATORS.get(operator_name).unwrap();
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            };
            out.write_all(UNARY_OPERATOR_SYNTAX.get(operator_name).unwrap().as_bytes())?;
            emit_js_under_operator(out, map, *argument, Some(operator.precedence))?;
            if must_parenthesise {
                out.write_all(b")")?;
            };
        }
        Syntax::UnaryPostfixExpr {
            parenthesised,
            operator: operator_name,
            argument,
        } => {
            let operator = OPERATORS.get(operator_name).unwrap();
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            };
            emit_js_under_operator(out, map, *argument, Some(operator.precedence))?;
            out.write_all(match operator_name {
                OperatorName::PostfixDecrement => b"--",
                OperatorName::PostfixIncrement => b"++",
                _ => unreachable!(),
            })?;
            if must_parenthesise {
                out.write_all(b")")?;
            };
        }
        Syntax::BlockStmt { body } => {
            out.write_all(b"{")?;
            emit_statements(out, map, &body)?;
            out.write_all(b"}")?;
        }
        Syntax::BreakStmt { label } => {
            out.write_all(b"break")?;
            if let Some(label) = label {
                out.write_all(b" ")?;
                out.write_all(label.as_slice())?;
            };
        }
        Syntax::ContinueStmt { label } => {
            out.write_all(b"continue")?;
            if let Some(label) = label {
                out.write_all(b" ")?;
                out.write_all(label.as_slice())?;
            };
        }
        Syntax::DebuggerStmt {} => {
            out.write_all(b"debugger")?;
        }
        Syntax::ComputedMemberExpr {
            optional_chaining,
            object,
            member,
        } => {
            emit_js_under_operator(
                out,
                map,
                *object,
                Some(OPERATORS[&OperatorName::ComputedMemberAccess].precedence),
            )?;
            if *optional_chaining {
                out.write_all(b"?.")?;
            };
            out.write_all(b"[")?;
            emit_js(out, map, *member)?;
            out.write_all(b"]")?;
        }
        // We split all `export class/function` into a declaration and an export at the end, so drop the `export`.
        // The exception is for unnamed functions and classes.
        Syntax::ExportDeclStmt {
            declaration,
            default,
        } => {
            match map[*declaration].stx() {
                Syntax::ClassDecl { name, .. } | Syntax::FunctionDecl { name, .. }
                    if name.is_none() =>
                {
                    debug_assert!(default);
                    out.write_all(b"export default ")?;
                }
                _ => {}
            };
            emit_js(out, map, *declaration)?;
        }
        Syntax::ExportDefaultExprStmt { expression } => {
            out.write_all(b"export default ")?;
            emit_js(out, map, *expression)?;
        }
        Syntax::ExportListStmt { names, from } => {
            out.write_all(b"export")?;
            emit_import_or_export_statement_trailer(out, map, Some(names), from.as_ref())?;
        }
        Syntax::ExpressionStmt { expression } => {
            emit_js(out, map, *expression)?;
        }
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => {
            out.write_all(b"if(")?;
            emit_js(out, map, *test)?;
            out.write_all(b")")?;
            emit_js(out, map, *consequent)?;
            if let Some(alternate) = alternate {
                if get_leaf_node_type(map, *consequent) == LeafNodeType::Block {
                    // Do nothing.
                } else {
                    out.write_all(b";")?;
                };
                out.write_all(b"else")?;
                if let Syntax::BlockStmt { .. } = map[*alternate].stx() {
                    // Do nothing.
                } else {
                    out.write_all(b" ")?;
                };
                emit_js(out, map, *alternate)?;
            };
        }
        Syntax::ForStmt { header, body } => {
            out.write_all(b"for(")?;
            match header {
                ForStmtHeader::Three {
                    init,
                    condition,
                    post,
                } => {
                    match init {
                        ForThreeInit::None => {}
                        ForThreeInit::Expression(n) | ForThreeInit::Declaration(n) => {
                            emit_js(out, map, *n)?
                        }
                    };
                    out.write_all(b";")?;
                    if let Some(n) = condition {
                        emit_js(out, map, *n)?
                    }
                    out.write_all(b";")?;
                    if let Some(n) = post {
                        emit_js(out, map, *n)?
                    }
                }
                ForStmtHeader::InOf { of, lhs, rhs } => {
                    match lhs {
                        ForInOfStmtHeaderLhs::Declaration(n) | ForInOfStmtHeaderLhs::Pattern(n) => {
                            emit_js(out, map, *n)?
                        }
                    };
                    if *of {
                        out.write_all(b" of ")?;
                    } else {
                        out.write_all(b" in ")?;
                    }
                    emit_js(out, map, *rhs)?;
                }
            };
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
        }
        Syntax::ImportStmt {
            default,
            names,
            module,
        } => {
            out.write_all(b"import")?;
            if let Some(default) = default {
                out.write_all(b" ")?;
                emit_js(out, map, *default)?;
                if names.is_some() {
                    out.write_all(b",")?;
                };
            };
            emit_import_or_export_statement_trailer(out, map, names.as_ref(), Some(module))?;
        }
        Syntax::ReturnStmt { value } => {
            out.write_all(b"return")?;
            if let Some(value) = value {
                // TODO Omit space if possible.
                out.write_all(b" ")?;
                emit_js(out, map, *value)?;
            };
        }
        Syntax::ThisExpr {} => {
            out.write_all(b"this")?;
        }
        Syntax::ThrowStmt { value } => {
            out.write_all(b"throw ")?;
            emit_js(out, map, *value)?;
        }
        Syntax::TopLevel { body } => {
            emit_statements(out, map, &body)?;
        }
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => {
            out.write_all(b"try")?;
            emit_js(out, map, *wrapped)?;
            if let Some(c) = catch {
                emit_js(out, map, *c)?;
            }
            if let Some(f) = finally {
                out.write_all(b"finally")?;
                emit_js(out, map, *f)?;
            };
        }
        Syntax::WhileStmt { condition, body } => {
            out.write_all(b"while(")?;
            emit_js(out, map, *condition)?;
            out.write_all(b")")?;
            emit_js(out, map, *body)?;
        }
        Syntax::DoWhileStmt { condition, body } => {
            out.write_all(b"do")?;
            if let Syntax::BlockStmt { .. } = map[*body].stx() {
                // Do nothing.
            } else {
                out.write_all(b" ")?;
            };
            emit_js(out, map, *body)?;
            if get_leaf_node_type(map, *body) == LeafNodeType::Block {
                // Do nothing.
            } else {
                out.write_all(b";")?;
            };
            out.write_all(b"while(")?;
            emit_js(out, map, *condition)?;
            out.write_all(b")")?;
        }
        Syntax::SwitchStmt { test, branches } => {
            out.write_all(b"switch(")?;
            emit_js(out, map, *test)?;
            out.write_all(b"){")?;
            for (i, b) in branches.iter().enumerate() {
                if i > 0 {
                    out.write_all(b";")?;
                };
                emit_js(out, map, *b)?;
            }
            out.write_all(b"}")?;
        }
        Syntax::CatchBlock { parameter, body } => {
            out.write_all(b"catch")?;
            if let Some(p) = parameter {
                out.write_all(b"(")?;
                emit_js(out, map, *p)?;
                out.write_all(b")")?;
            }
            emit_js(out, map, *body)?;
        }
        Syntax::SwitchBranch { case, body } => {
            match case {
                Some(case) => {
                    // TODO Omit space if possible.
                    out.write_all(b"case ")?;
                    emit_js(out, map, *case)?;
                    out.write_all(b":")?;
                }
                None => {
                    out.write_all(b"default:")?;
                }
            }
            emit_statements(out, map, &body)?;
        }
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => {
            match key {
                ClassOrObjectMemberKey::Direct(name) => {
                    out.write_all(name.as_slice())?;
                }
                ClassOrObjectMemberKey::Computed(expr) => {
                    out.write_all(b"[")?;
                    emit_js(out, map, *expr)?;
                    out.write_all(b"]")?;
                }
            };
            if let Some(t) = target {
                out.write_all(b":")?;
                emit_js(out, map, *t)?;
            };
            if let Some(v) = default_value {
                out.write_all(b"=")?;
                emit_js(out, map, *v)?;
            };
        }
        Syntax::ObjectMember { typ } => {
            match typ {
                ObjectMemberType::Valued { key, value } => {
                    emit_class_or_object_member(out, map, key, value, b":")?;
                }
                ObjectMemberType::Shorthand { name } => {
                    out.write_all(name.as_slice())?;
                }
                ObjectMemberType::Rest { value } => {
                    out.write_all(b"...")?;
                    emit_js(out, map, *value)?;
                }
            };
        }
        Syntax::MemberExpr {
            parenthesised,
            optional_chaining,
            left,
            right,
        } => {
            let operator_name = &if *optional_chaining {
                OperatorName::OptionalChainingMemberAccess
            } else {
                OperatorName::MemberAccess
            };
            let operator = &OPERATORS[operator_name];
            let must_parenthesise = match parent_operator_precedence {
                Some(po) if po > operator.precedence => true,
                Some(po) if po == operator.precedence => *parenthesised,
                _ => false,
            };
            if must_parenthesise {
                out.write_all(b"(")?;
            };
            emit_js_under_operator(out, map, *left, Some(operator.precedence))?;
            out.write_all(
                BINARY_OPERATOR_SYNTAX
                    .get(operator_name)
                    .unwrap()
                    .as_bytes(),
            )?;
            out.write_all(right.as_slice())?;
            if must_parenthesise {
                out.write_all(b")")?;
            };
        }
        Syntax::ClassExpr {
            parenthesised,
            name,
            extends,
            members,
        } => {
            // We need to keep parentheses to prevent class expressions from being misinterpreted as a class declaration, which cannot be part of an expression.
            // TODO Omit parentheses if possible.
            if *parenthesised {
                out.write_all(b"(")?;
            }
            emit_class(out, map, *name, *extends, members)?;
            // TODO Omit parentheses if possible.
            if *parenthesised {
                out.write_all(b")")?;
            }
        }
        Syntax::LabelStmt { name, statement } => {
            out.write_all(name.as_slice())?;
            out.write_all(b":")?;
            emit_js(out, map, *statement)?;
        }
        Syntax::CallArg { spread, value } => {
            if *spread {
                out.write_all(b"...")?;
            }
            emit_js(out, map, *value)?;
        }
        Syntax::SuperExpr {} => {
            out.write_all(b"super")?;
        }
    };
    Ok(())
}
