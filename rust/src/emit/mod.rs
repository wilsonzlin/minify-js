use lazy_static::lazy_static;
use parse_js::ast::ArrayElement;
use parse_js::ast::ClassMember;
use parse_js::ast::ClassOrObjectMemberKey;
use parse_js::ast::ClassOrObjectMemberValue;
use parse_js::ast::ExportNames;
use parse_js::ast::ForInOfStmtHeaderLhs;
use parse_js::ast::ForStmtHeader;
use parse_js::ast::ForThreeInit;
use parse_js::ast::LiteralTemplatePart;
use parse_js::ast::NodeData;
use parse_js::ast::ObjectMemberType;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::operator::OperatorName;
use parse_js::operator::OPERATORS;
use parse_js::session::SessionString;
use parse_js::session::SessionVec;
use std::collections::HashMap;
use std::io::Write;

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
fn emit_class_or_object_member<'a>(
  out: &mut Vec<u8>,
  key: &'a ClassOrObjectMemberKey,
  value: &'a ClassOrObjectMemberValue,
  value_delimiter: &'static [u8],
) -> bool {
  let is_computed_key = match key {
    ClassOrObjectMemberKey::Computed(_) => true,
    _ => false,
  };
  match value {
    ClassOrObjectMemberValue::Getter { .. } => {
      out.extend_from_slice(b"get");
      if !is_computed_key {
        out.extend_from_slice(b" ");
      };
    }
    ClassOrObjectMemberValue::Setter { .. } => {
      out.extend_from_slice(b"set");
      if !is_computed_key {
        out.extend_from_slice(b" ");
      };
    }
    ClassOrObjectMemberValue::Method {
      is_async,
      generator,
      ..
    } => {
      if *is_async {
        out.extend_from_slice(b"async");
      }
      if *generator {
        out.extend_from_slice(b"*");
      } else if *is_async {
        out.extend_from_slice(b" ");
      }
    }
    _ => {}
  };
  match key {
    ClassOrObjectMemberKey::Direct(name) => {
      out.extend_from_slice(name.as_slice());
    }
    ClassOrObjectMemberKey::Computed(expr) => {
      out.extend_from_slice(b"[");
      emit_js(out, *expr);
      out.extend_from_slice(b"]");
    }
  };
  match value {
    ClassOrObjectMemberValue::Getter { body } => {
      out.extend_from_slice(b"()");
      emit_js(out, *body);
    }
    ClassOrObjectMemberValue::Method {
      signature, body, ..
    } => {
      out.extend_from_slice(b"(");
      emit_js(out, *signature);
      out.extend_from_slice(b")");
      emit_js(out, *body);
    }
    ClassOrObjectMemberValue::Property { initializer } => {
      if let Some(v) = initializer {
        out.extend_from_slice(value_delimiter);
        let is_comma = is_comma_expression(&v.stx);
        if is_comma {
          out.extend_from_slice(b"(");
        };
        emit_js(out, *v);
        if is_comma {
          out.extend_from_slice(b")");
        };
      };
    }
    ClassOrObjectMemberValue::Setter { body, parameter } => {
      out.extend_from_slice(b"(");
      emit_js(out, *parameter);
      out.extend_from_slice(b")");
      emit_js(out, *body);
    }
  };

  match value {
    ClassOrObjectMemberValue::Property { .. } => true,
    _ => false,
  }
}

fn emit_class<'a>(
  out: &mut Vec<u8>,
  name: &Option<&mut NodeData<'a>>,
  extends: &Option<&mut NodeData<'a>>,
  members: &SessionVec<'a, ClassMember<'a>>,
) -> () {
  out.extend_from_slice(b"class");
  if let Some(n) = name {
    out.extend_from_slice(b" ");
    emit_js(out, n);
  }
  if let Some(s) = extends {
    out.extend_from_slice(b" extends ");
    emit_js(out, s);
  }
  out.extend_from_slice(b"{");
  let mut last_member_was_property = false;
  for (i, m) in members.iter().enumerate() {
    if i > 0 && last_member_was_property {
      out.extend_from_slice(b";");
    }
    if m.statik {
      out.extend_from_slice(b"static ");
    }
    last_member_was_property = emit_class_or_object_member(out, &m.key, &m.value, b"=");
  }
  out.extend_from_slice(b"}");
}

fn emit_import_or_export_statement_trailer<'a>(
  out: &mut Vec<u8>,
  names: Option<&ExportNames<'a>>,
  from: Option<&SessionString<'a>>,
) -> () {
  match names {
    Some(ExportNames::All(alias)) => {
      out.extend_from_slice(b"*");
      if let Some(alias) = alias {
        out.extend_from_slice(b"as ");
        emit_js(out, *alias);
        if from.is_some() {
          out.extend_from_slice(b" ");
        }
      };
    }
    Some(ExportNames::Specific(names)) => {
      out.extend_from_slice(b"{");
      for (i, e) in names.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        out.extend_from_slice(e.target.as_slice());
        // TODO Omit if identical to `target`.
        out.extend_from_slice(b" as ");
        emit_js(out, e.alias);
      }
      out.extend_from_slice(b"}");
    }
    None => {}
  };
  if let Some(from) = from {
    out.extend_from_slice(b"from\"");
    // TODO Escape?
    out.extend_from_slice(from.as_bytes());
    out.extend_from_slice(b"\"");
  };
}

// NOTE: We no longer support outputting to a generic Write, as that incurs significant performance overhead (even with a BufWriter<Vec<u8>>) and our parser is not streaming anyway.
pub fn emit_js<'a>(out: &mut Vec<u8>, n: &NodeData<'a>) -> () {
  emit_js_under_operator(out, n, None);
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LeafNodeType {
  EmptyStmt,
  Other,
  Block,
}

fn get_leaf_node_type<'a>(n: &NodeData<'a>) -> LeafNodeType {
  match &n.stx {
    Syntax::WhileStmt { body, .. } | Syntax::ForStmt { body, .. } => get_leaf_node_type(*body),
    Syntax::LabelStmt { statement, .. } => get_leaf_node_type(*statement),
    Syntax::IfStmt {
      consequent,
      alternate,
      ..
    } => match alternate {
      Some(n) => get_leaf_node_type(*n),
      None => get_leaf_node_type(*consequent),
    },
    Syntax::BlockStmt { .. } => LeafNodeType::Block,
    Syntax::EmptyStmt {} => LeafNodeType::EmptyStmt,
    Syntax::TryStmt { .. } => LeafNodeType::Block,
    Syntax::SwitchStmt { .. } => LeafNodeType::Block,
    _ => LeafNodeType::Other,
  }
}

// It's important to use this function:
// - Omit semicolons where possible.
// - Insert semicolon after last statement if its leaf is a `if`, `for`, `while`, or `with` statement with an empty statement as its body e.g. `if (x) label: for (;;) while (x)` but not `if (x) for (;;) label: while (x) {}` or `if (x) for (;;) label: while (x) return`.
fn emit_statements<'a>(out: &mut Vec<u8>, statements: &[&mut NodeData<'a>]) -> () {
  // Since we skip over some statements, the last actual statement may not be the last in the list.
  let mut last_statement: Option<&NodeData<'a>> = None;
  for n in statements {
    if let Syntax::EmptyStmt {} = n.stx {
      continue;
    };
    if let Some(n) = last_statement {
      match &n.stx {
        Syntax::BlockStmt { .. }
        | Syntax::ClassDecl { .. }
        | Syntax::EmptyStmt {}
        | Syntax::FunctionDecl { .. }
        | Syntax::SwitchStmt { .. }
        | Syntax::TryStmt { .. } => {}
        _ => out.extend_from_slice(b";"),
      }
    }
    emit_js(out, *n);
    last_statement = Some(*n);
  }
  if let Some(n) = last_statement {
    if get_leaf_node_type(n) == LeafNodeType::EmptyStmt {
      out.extend_from_slice(b";");
    }
  }
}

fn is_comma_expression<'a>(stx: &Syntax<'a>) -> bool {
  match stx {
    Syntax::BinaryExpr { operator, .. } => *operator == OperatorName::Comma,
    _ => false,
  }
}

fn leftmost_expression<'a, 'b>(stx: &'b Syntax<'a>) -> &'b Syntax<'a> {
  match stx {
    Syntax::ComputedMemberExpr { object, .. } => leftmost_expression(&object.stx),
    Syntax::MemberExpr { left, .. } | Syntax::BinaryExpr { left, .. } => {
      leftmost_expression(&left.stx)
    }
    _ => stx,
  }
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

fn emit_js_under_operator<'a>(
  out: &mut Vec<u8>,
  node: &NodeData<'a>,
  parent_operator_precedence: Option<u8>,
) -> () {
  match &node.stx {
    Syntax::EmptyStmt {} => {}
    Syntax::LiteralBigIntExpr { .. } => {
      // TODO This is invalid as `loc` may not be valid (e.g. newly created node during transform).
      out.extend_from_slice(node.loc.as_slice());
    }
    Syntax::LiteralRegexExpr { .. } => {
      // TODO This is invalid as `loc` may not be valid (e.g. newly created node during transform).
      out.extend_from_slice(node.loc.as_slice());
    }
    Syntax::LiteralBooleanExpr { value } => {
      match *value {
        true => out.extend_from_slice(b"!0"),
        false => out.extend_from_slice(b"!1"),
      };
    }
    Syntax::LiteralNumberExpr { value } => {
      // TODO Possibly invalid.
      write!(out, "{}", value).unwrap();
    }
    Syntax::LiteralStringExpr { value } => {
      // TODO Possibly invalid.
      write!(
        out,
        "`{}`",
        value
          .replace("\\", "\\\\")
          .replace("`", "\\`")
          .replace("$", "\\$")
      )
      .unwrap();
    }
    Syntax::LiteralTemplateExpr { parts } => {
      out.extend_from_slice(b"`");
      for p in parts {
        match p {
          LiteralTemplatePart::Substitution(sub) => {
            out.extend_from_slice(b"${");
            emit_js(out, *sub);
            out.extend_from_slice(b"}");
          }
          LiteralTemplatePart::String(str) => {
            out.extend_from_slice(str.as_slice());
          }
        }
      }
      out.extend_from_slice(b"`");
    }
    Syntax::VarDecl {
      mode, declarators, ..
    } => {
      // We split all `export var/let/const` into a declaration and an export at the end, so drop the `export`.
      out.extend_from_slice(match mode {
        VarDeclMode::Const => b"const",
        VarDeclMode::Let => b"let",
        VarDeclMode::Var => b"var",
      });
      out.extend_from_slice(b" ");
      for (i, decl) in declarators.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        emit_js(out, decl.pattern);
        if let Some(expr) = &decl.initializer {
          out.extend_from_slice(b"=");
          // This is only really done for the Comma operator, which is the only operator below Assignment.
          let operator = &OPERATORS[&OperatorName::Assignment];
          emit_js_under_operator(out, *expr, Some(operator.precedence));
        };
      }
    }
    Syntax::VarStmt { declaration } => {
      emit_js(out, *declaration);
    }
    Syntax::IdentifierPattern { name } => {
      out.extend_from_slice(name.as_slice());
    }
    Syntax::ArrayPattern { elements, rest } => {
      out.extend_from_slice(b"[");
      for (i, e) in elements.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        if let Some(e) = e {
          emit_js(out, e.target);
          if let Some(v) = &e.default_value {
            out.extend_from_slice(b"=");
            emit_js(out, *v);
          }
        };
      }
      if let Some(r) = rest {
        if !elements.is_empty() {
          out.extend_from_slice(b",");
        }
        out.extend_from_slice(b"...");
        emit_js(out, *r);
      };
      out.extend_from_slice(b"]");
    }
    Syntax::ObjectPattern { properties, rest } => {
      out.extend_from_slice(b"{");
      for (i, e) in properties.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        emit_js(out, *e);
      }
      if let Some(r) = rest {
        if !properties.is_empty() {
          out.extend_from_slice(b",");
        }
        out.extend_from_slice(b"...");
        emit_js(out, *r);
      };
      out.extend_from_slice(b"}");
    }
    Syntax::ClassOrFunctionName { name } => {
      out.extend_from_slice(name.as_slice());
    }
    Syntax::FunctionSignature { parameters } => {
      for (i, p) in parameters.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        };
        emit_js(out, *p);
      }
    }
    Syntax::ClassDecl {
      export,
      export_default,
      name,
      extends,
      members,
    } => {
      // We split all `export class/function` into a declaration and an export at the end, so drop the `export`.
      // The exception is for unnamed functions and classes.
      if *export && name.is_none() {
        debug_assert!(*export_default);
        out.extend_from_slice(b"export default ");
      }
      emit_class(out, name, extends, members);
    }
    Syntax::FunctionDecl {
      export,
      export_default,
      is_async,
      generator,
      name,
      signature,
      body,
    } => {
      // We split all `export class/function` into a declaration and an export at the end, so drop the `export`.
      // The exception is for unnamed functions and classes.
      if *export && name.is_none() {
        debug_assert!(*export_default);
        out.extend_from_slice(b"export default ");
      }
      if *is_async {
        out.extend_from_slice(b"async ");
      }
      out.extend_from_slice(b"function");
      if *generator {
        out.extend_from_slice(b"*");
      } else if name.is_some() {
        out.extend_from_slice(b" ");
      };
      if let Some(name) = name {
        emit_js(out, *name);
      }
      out.extend_from_slice(b"(");
      emit_js(out, *signature);
      out.extend_from_slice(b")");
      emit_js(out, *body);
    }
    Syntax::ParamDecl {
      rest,
      pattern,
      default_value,
    } => {
      if *rest {
        out.extend_from_slice(b"...");
      };
      emit_js(out, *pattern);
      if let Some(v) = default_value {
        out.extend_from_slice(b"=");
        emit_js(out, *v);
      }
    }
    Syntax::ArrowFunctionExpr {
      parenthesised,
      is_async,
      signature,
      body,
    } => {
      // See FunctionExpr.
      // TODO Omit parentheses if possible.
      if *parenthesised {
        out.extend_from_slice(b"(");
      }
      if *is_async {
        out.extend_from_slice(b"async");
      }
      let can_omit_parentheses = if let Syntax::FunctionSignature { parameters } = &signature.stx {
        !is_async
          && parameters.len() == 1
          && match &parameters[0].stx {
            Syntax::ParamDecl {
              default_value,
              pattern,
              rest,
            } => {
              !rest
                && default_value.is_none()
                && match &pattern.stx {
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
        out.extend_from_slice(b"(");
      };
      emit_js(out, *signature);
      if !can_omit_parentheses {
        out.extend_from_slice(b")");
      };
      out.extend_from_slice(b"=>");
      let must_parenthesise_body = match &body.stx {
        expr if is_comma_expression(expr) => true,
        // `{a: b}.b`, `{a: b} + 1`, etc. need to be wrapped.
        // TODO Refine and verify.
        expr => match leftmost_expression(expr) {
          Syntax::LiteralObjectExpr { .. } => true,
          _ => false,
        },
      };
      if must_parenthesise_body {
        out.extend_from_slice(b"(");
      };
      emit_js(out, *body);
      if must_parenthesise_body {
        out.extend_from_slice(b")");
      };
      // TODO Omit parentheses if possible.
      if *parenthesised {
        out.extend_from_slice(b")");
      };
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
        out.extend_from_slice(b"(");
      };
      emit_js_under_operator(out, *left, Some(operator.precedence));
      out.extend_from_slice(
        BINARY_OPERATOR_SYNTAX
          .get(operator_name)
          .unwrap()
          .as_bytes(),
      );
      match operator_name {
        OperatorName::Addition | OperatorName::Subtraction => {
          // Prevent potential confict with following unary operator e.g. `a+ +b` => `a++b`.
          // TODO Omit when possible.
          out.extend_from_slice(b" ");
        }
        _ => {}
      };
      emit_js_under_operator(out, *right, Some(operator.precedence));
      if must_parenthesise {
        out.extend_from_slice(b")");
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
        out.extend_from_slice(b"(");
      }
      emit_js_under_operator(out, *callee, Some(operator.precedence));
      if *optional_chaining {
        out.extend_from_slice(b"?.");
      }
      out.extend_from_slice(b"(");
      for (i, a) in arguments.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        emit_js(out, *a);
      }
      out.extend_from_slice(b")");
      // TODO Omit parentheses if possible.
      if must_parenthesise {
        out.extend_from_slice(b")");
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
        out.extend_from_slice(b"(");
      };
      emit_js_under_operator(out, *test, Some(operator.precedence));
      out.extend_from_slice(b"?");
      emit_js_under_operator(out, *consequent, Some(operator.precedence));
      out.extend_from_slice(b":");
      emit_js_under_operator(out, *alternate, Some(operator.precedence));
      if must_parenthesise {
        out.extend_from_slice(b")");
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
        out.extend_from_slice(b"(");
      }
      if *is_async {
        out.extend_from_slice(b"async ");
      }
      out.extend_from_slice(b"function");
      if *generator {
        out.extend_from_slice(b"*");
      };
      if let Some(name) = name {
        if !generator {
          out.extend_from_slice(b" ");
        };
        emit_js(out, *name);
      };
      out.extend_from_slice(b"(");
      emit_js(out, *signature);
      out.extend_from_slice(b")");
      emit_js(out, *body);
      // TODO Omit parentheses if possible.
      if *parenthesised {
        out.extend_from_slice(b")");
      }
    }
    Syntax::IdentifierExpr { name } => {
      out.extend_from_slice(name.as_slice());
    }
    Syntax::ImportExpr { module } => {
      out.extend_from_slice(b"import(");
      emit_js(out, *module);
      out.extend_from_slice(b")");
    }
    Syntax::ImportMeta {} => {
      out.extend_from_slice(b"import.meta");
    }
    Syntax::JsxAttribute { name, value } => {
      emit_js(out, *name);
      if let Some(value) = value {
        out.extend_from_slice(b"=");
        emit_js(out, *value);
      }
    }
    Syntax::JsxElement {
      name,
      attributes,
      children,
    } => {
      out.extend_from_slice(b"<");
      if let Some(name) = name {
        emit_js(out, *name);
      }
      for attr in attributes {
        out.extend_from_slice(b" ");
        emit_js(out, *attr);
      }
      if children.is_empty() {
        out.extend_from_slice(b"/>");
      } else {
        out.extend_from_slice(b">");
        for child in children {
          emit_js(out, *child);
        }
        out.extend_from_slice(b"</");
        if let Some(name) = name {
          emit_js(out, *name);
        }
        out.extend_from_slice(b">");
      }
    }
    Syntax::JsxExpressionContainer { value } => {
      out.extend_from_slice(b"{");
      emit_js(out, *value);
      out.extend_from_slice(b"}");
    }
    Syntax::JsxMember { base, path } => {
      out.extend_from_slice(base.as_slice());
      for c in path {
        out.extend_from_slice(b".");
        out.extend_from_slice(c.as_slice());
      }
    }
    Syntax::JsxName { namespace, name } => {
      if let Some(namespace) = namespace {
        out.extend_from_slice(namespace.as_slice());
        out.extend_from_slice(b":");
      }
      out.extend_from_slice(name.as_slice());
    }
    Syntax::JsxSpreadAttribute { value } => {
      out.extend_from_slice(b"{...");
      emit_js(out, *value);
      out.extend_from_slice(b"}");
    }
    Syntax::JsxText { value } => {
      out.extend_from_slice(value.as_slice());
    }
    Syntax::LiteralArrayExpr { elements } => {
      out.extend_from_slice(b"[");
      for (i, e) in elements.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        };
        match e {
          ArrayElement::Single(expr) => {
            emit_js(out, *expr);
          }
          ArrayElement::Rest(expr) => {
            out.extend_from_slice(b"...");
            emit_js(out, *expr);
          }
          ArrayElement::Empty => {}
        };
      }
      out.extend_from_slice(b"]");
    }
    Syntax::LiteralObjectExpr { members } => {
      out.extend_from_slice(b"{");
      for (i, e) in members.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b",");
        }
        emit_js(out, *e);
      }
      out.extend_from_slice(b"}");
    }
    Syntax::LiteralNull {} => {
      out.extend_from_slice(b"null");
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
        out.extend_from_slice(b"(");
      };
      out.extend_from_slice(UNARY_OPERATOR_SYNTAX.get(operator_name).unwrap().as_bytes());
      emit_js_under_operator(out, *argument, Some(operator.precedence));
      if must_parenthesise {
        out.extend_from_slice(b")");
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
        out.extend_from_slice(b"(");
      };
      emit_js_under_operator(out, *argument, Some(operator.precedence));
      out.extend_from_slice(match operator_name {
        OperatorName::PostfixDecrement => b"--",
        OperatorName::PostfixIncrement => b"++",
        _ => unreachable!(),
      });
      if must_parenthesise {
        out.extend_from_slice(b")");
      };
    }
    Syntax::BlockStmt { body } => {
      out.extend_from_slice(b"{");
      emit_statements(out, &body);
      out.extend_from_slice(b"}");
    }
    Syntax::BreakStmt { label } => {
      out.extend_from_slice(b"break");
      if let Some(label) = label {
        out.extend_from_slice(b" ");
        out.extend_from_slice(label.as_slice());
      };
    }
    Syntax::ContinueStmt { label } => {
      out.extend_from_slice(b"continue");
      if let Some(label) = label {
        out.extend_from_slice(b" ");
        out.extend_from_slice(label.as_slice());
      };
    }
    Syntax::DebuggerStmt {} => {
      out.extend_from_slice(b"debugger");
    }
    Syntax::ComputedMemberExpr {
      optional_chaining,
      object,
      member,
      ..
    } => {
      emit_js_under_operator(
        out,
        *object,
        Some(OPERATORS[&OperatorName::ComputedMemberAccess].precedence),
      );
      if *optional_chaining {
        out.extend_from_slice(b"?.");
      };
      out.extend_from_slice(b"[");
      emit_js(out, *member);
      out.extend_from_slice(b"]");
    }
    Syntax::ExportDefaultExprStmt { expression } => {
      out.extend_from_slice(b"export default ");
      emit_js(out, *expression);
    }
    Syntax::ExportListStmt { names, from } => {
      out.extend_from_slice(b"export");
      emit_import_or_export_statement_trailer(out, Some(names), from.as_ref());
    }
    Syntax::ExpressionStmt { expression } => {
      emit_js(out, *expression);
    }
    Syntax::IfStmt {
      test,
      consequent,
      alternate,
    } => {
      out.extend_from_slice(b"if(");
      emit_js(out, *test);
      out.extend_from_slice(b")");
      emit_js(out, *consequent);
      if let Some(alternate) = alternate {
        if get_leaf_node_type(*consequent) == LeafNodeType::Block {
          // Do nothing.
        } else {
          out.extend_from_slice(b";");
        };
        out.extend_from_slice(b"else");
        if let Syntax::BlockStmt { .. } = &alternate.stx {
          // Do nothing.
        } else {
          out.extend_from_slice(b" ");
        };
        emit_js(out, *alternate);
      };
    }
    Syntax::ForStmt { header, body } => {
      out.extend_from_slice(b"for(");
      match header {
        ForStmtHeader::Three {
          init,
          condition,
          post,
        } => {
          match init {
            ForThreeInit::None => {}
            ForThreeInit::Expression(n) | ForThreeInit::Declaration(n) => emit_js(out, *n),
          };
          out.extend_from_slice(b";");
          if let Some(n) = condition {
            emit_js(out, *n);
          };
          out.extend_from_slice(b";");
          if let Some(n) = post {
            emit_js(out, *n);
          };
        }
        ForStmtHeader::InOf { of, lhs, rhs } => {
          match lhs {
            ForInOfStmtHeaderLhs::Declaration(n) | ForInOfStmtHeaderLhs::Pattern(n) => {
              emit_js(out, *n);
            }
          };
          if *of {
            out.extend_from_slice(b" of ");
          } else {
            out.extend_from_slice(b" in ");
          }
          emit_js(out, *rhs);
        }
      };
      out.extend_from_slice(b")");
      emit_js(out, *body);
    }
    Syntax::ImportStmt {
      default,
      names,
      module,
    } => {
      out.extend_from_slice(b"import");
      if let Some(default) = default {
        out.extend_from_slice(b" ");
        emit_js(out, *default);
        if names.is_some() {
          out.extend_from_slice(b",");
        } else {
          out.extend_from_slice(b" ");
        };
      };
      emit_import_or_export_statement_trailer(out, names.as_ref(), Some(module));
    }
    Syntax::ReturnStmt { value } => {
      out.extend_from_slice(b"return");
      if let Some(value) = value {
        // TODO Omit space if possible.
        out.extend_from_slice(b" ");
        emit_js(out, *value);
      };
    }
    Syntax::ThisExpr {} => {
      out.extend_from_slice(b"this");
    }
    Syntax::ThrowStmt { value } => {
      out.extend_from_slice(b"throw ");
      emit_js(out, *value);
    }
    Syntax::TopLevel { body } => {
      emit_statements(out, &body);
    }
    Syntax::TryStmt {
      wrapped,
      catch,
      finally,
    } => {
      out.extend_from_slice(b"try");
      emit_js(out, *wrapped);
      if let Some(c) = catch {
        emit_js(out, *c);
      }
      if let Some(f) = finally {
        out.extend_from_slice(b"finally");
        emit_js(out, *f);
      };
    }
    Syntax::WhileStmt { condition, body } => {
      out.extend_from_slice(b"while(");
      emit_js(out, *condition);
      out.extend_from_slice(b")");
      emit_js(out, *body);
    }
    Syntax::DoWhileStmt { condition, body } => {
      out.extend_from_slice(b"do");
      if let Syntax::BlockStmt { .. } = &body.stx {
        // Do nothing.
      } else {
        out.extend_from_slice(b" ");
      };
      emit_js(out, *body);
      if get_leaf_node_type(*body) == LeafNodeType::Block {
        // Do nothing.
      } else {
        out.extend_from_slice(b";");
      };
      out.extend_from_slice(b"while(");
      emit_js(out, *condition);
      out.extend_from_slice(b")");
    }
    Syntax::SwitchStmt { test, branches } => {
      out.extend_from_slice(b"switch(");
      emit_js(out, *test);
      out.extend_from_slice(b"){");
      for (i, b) in branches.iter().enumerate() {
        if i > 0 {
          out.extend_from_slice(b";");
        };
        emit_js(out, *b);
      }
      out.extend_from_slice(b"}");
    }
    Syntax::CatchBlock { parameter, body } => {
      out.extend_from_slice(b"catch");
      if let Some(p) = parameter {
        out.extend_from_slice(b"(");
        emit_js(out, *p);
        out.extend_from_slice(b")");
      }
      emit_js(out, *body);
    }
    Syntax::SwitchBranch { case, body } => {
      match case {
        Some(case) => {
          // TODO Omit space if possible.
          out.extend_from_slice(b"case ");
          emit_js(out, *case);
          out.extend_from_slice(b":");
        }
        None => {
          out.extend_from_slice(b"default:");
        }
      }
      emit_statements(out, &body);
    }
    Syntax::ObjectPatternProperty {
      key,
      target,
      default_value,
    } => {
      match key {
        ClassOrObjectMemberKey::Direct(name) => {
          out.extend_from_slice(name.as_slice());
        }
        ClassOrObjectMemberKey::Computed(expr) => {
          out.extend_from_slice(b"[");
          emit_js(out, *expr);
          out.extend_from_slice(b"]");
        }
      };
      if let Some(t) = target {
        out.extend_from_slice(b":");
        emit_js(out, *t);
      };
      if let Some(v) = default_value {
        out.extend_from_slice(b"=");
        emit_js(out, *v);
      };
    }
    Syntax::ObjectMember { typ } => {
      match typ {
        ObjectMemberType::Valued { key, value } => {
          emit_class_or_object_member(out, key, value, b":");
        }
        ObjectMemberType::Shorthand { name } => {
          out.extend_from_slice(name.as_slice());
        }
        ObjectMemberType::Rest { value } => {
          out.extend_from_slice(b"...");
          emit_js(out, *value);
        }
      };
    }
    Syntax::MemberExpr {
      parenthesised,
      optional_chaining,
      left,
      right,
      ..
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
        out.extend_from_slice(b"(");
      };
      emit_js_under_operator(out, *left, Some(operator.precedence));
      out.extend_from_slice(
        BINARY_OPERATOR_SYNTAX
          .get(operator_name)
          .unwrap()
          .as_bytes(),
      );
      out.extend_from_slice(right.as_slice());
      if must_parenthesise {
        out.extend_from_slice(b")");
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
        out.extend_from_slice(b"(");
      }
      emit_class(out, name, extends, members);
      // TODO Omit parentheses if possible.
      if *parenthesised {
        out.extend_from_slice(b")");
      }
    }
    Syntax::LabelStmt { name, statement } => {
      out.extend_from_slice(name.as_slice());
      out.extend_from_slice(b":");
      emit_js(out, *statement);
    }
    Syntax::CallArg { spread, value } => {
      if *spread {
        out.extend_from_slice(b"...");
      }
      emit_js(out, *value);
    }
    Syntax::SuperExpr {} => {
      out.extend_from_slice(b"super");
    }
    Syntax::_TakenNode {} => unreachable!(),
  };
}
