use serde_json::{json, Value};

use crate::ast::{
    ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ForInOfStmtHeaderLhs,
    ForStmtHeader, ForThreeInit, LiteralTemplatePart, NodeId, NodeMap, ObjectMemberType, Syntax,
};

fn visit_class_or_object_member_key(m: &NodeMap, key: &ClassOrObjectMemberKey) -> Value {
    match key {
        ClassOrObjectMemberKey::Direct(r) => json!(r.as_str().to_string()),
        ClassOrObjectMemberKey::Computed(e) => visit_node(m, *e),
    }
}

fn visit_class_or_object_member_value(m: &NodeMap, value: &ClassOrObjectMemberValue) -> Value {
    match value {
        ClassOrObjectMemberValue::Getter { body } => json!({
            "getter": json!({
                "body": visit_node(m, *body),
            }),
        }),
        ClassOrObjectMemberValue::Method {
            is_async,
            generator,
            signature,
            body,
        } => json!({
            "method": json!({
                "async": is_async,
                "generator": generator,
                "signature": visit_node(m, *signature),
                "body": visit_node(m, *body),
            }),
        }),
        ClassOrObjectMemberValue::Property { initializer } => json!({
            "property": json!({
                "initializer": initializer.map(|n| visit_node(m, n)),
            }),
        }),
        ClassOrObjectMemberValue::Setter { parameter, body } => json!({
            "setter": json!({
                "parameter": visit_node(m, *parameter),
                "body": visit_node(m, *body),
            }),
        }),
    }
}

fn visit_node(m: &NodeMap, n: NodeId) -> Value {
    match m[n].stx() {
        Syntax::IdentifierPattern { name } => json!({
            "$t": "IdentifierPattern",
            "name": name.as_str().to_string(),
        }),
        Syntax::ArrayPattern { elements, rest } => json!({
            "$t": "ArrayPattern",
            "elements": elements.iter().map(|e| e.as_ref().map(|e| json!({
                "target": visit_node(m, e.target),
                "default_value": e.default_value.map(|n| visit_node(m, n)),
            }))).collect::<Vec<_>>(),
            "rest": rest.map(|n| visit_node(m, n)),
        }),
        Syntax::ObjectPattern { properties, rest } => json!({
            "$t": "ObjectPattern",
            "properties": properties.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
            "rest": rest.map(|n| visit_node(m, n)),
        }),
        Syntax::ClassOrFunctionName { name } => json!({
            "$t": "FunctionName",
            "name": name.as_str().to_string(),
        }),
        Syntax::FunctionSignature { parameters } => json!({
            "$t": "FunctionSignature",
            "parameters": parameters.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ClassDecl {
            name,
            extends,
            members,
        } => json!({
            "$t": "ClassDecl",
            "name": name.map(|n| visit_node(m, n)),
            "extends": extends.map(|n| visit_node(m, n)),
            "members": members.iter().map(|mem| json!({
                "static": mem.statik,
                "key": visit_class_or_object_member_key(m, &mem.key),
                "value": visit_class_or_object_member_value(m, &mem.value),
            })).collect::<Vec<_>>(),
        }),
        Syntax::ClassExpr {
            parenthesised,
            name,
            extends,
            members,
        } => json!({
            "$t": "ClassExpr",
            "parenthesised": parenthesised,
            "name": name.map(|n| visit_node(m, n)),
            "extends": extends.map(|n| visit_node(m, n)),
            "members": members.iter().map(|mem| json!({
                "static": mem.statik,
                "key": visit_class_or_object_member_key(m, &mem.key),
                "value": visit_class_or_object_member_value(m, &mem.value),
            })).collect::<Vec<_>>(),
        }),
        Syntax::FunctionDecl {
            is_async,
            generator,
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionDecl",
            "async": is_async,
            "generator": generator,
            "name": name.map(|n| visit_node(m, n)),
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::ParamDecl {
            rest,
            pattern,
            default_value,
        } => json!({
            "$t": "ParamDecl",
            "rest": rest,
            "pattern": visit_node(m, *pattern),
            "default_value": default_value.map(|n| visit_node(m, n)),
        }),
        Syntax::VarDecl { mode, declarators } => json!({
            "$t": "VarDecl",
            "mode": mode,
            "declarators": declarators.iter().map(|d| json!({
                "pattern": visit_node(m, d.pattern),
                "initializer": d.initializer.map(|n| visit_node(m, n)),
            })).collect::<Vec<_>>(),
        }),
        Syntax::ArrowFunctionExpr {
            is_async,
            signature,
            body,
        } => json!({
            "$t": "ArrowFunctionExpr",
            "async": is_async,
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::BinaryExpr {
            parenthesised,
            operator,
            left,
            right,
        } => json!({
            "$t": "BinaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "left": visit_node(m, *left),
            "right": visit_node(m, *right),
        }),
        Syntax::CallExpr {
            parenthesised,
            optional_chaining,
            callee,
            arguments,
        } => json!({
            "$t": "CallExpr",
            "optional_chaining": optional_chaining,
            "parenthesised": parenthesised,
            "callee": visit_node(m, *callee),
            "arguments": arguments.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ConditionalExpr {
            parenthesised,
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "ConditionalExpr",
            "parenthesised": parenthesised,
            "test": visit_node(m, *test),
            "consequent": visit_node(m, *consequent),
            "alternate": visit_node(m, *alternate),
        }),
        Syntax::ComputedMemberExpr {
            optional_chaining,
            object,
            member,
        } => json!({
            "$t": "ComputedMemberExpr",
            "optional_chaining": optional_chaining,
            "object": visit_node(m, *object),
            "member": visit_node(m, *member),
        }),
        Syntax::FunctionExpr {
            parenthesised,
            is_async,
            generator,
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionExpr",
            "parenthesised": parenthesised,
            "async": is_async,
            "generator": generator,
            "name": name.as_ref().map(|n| visit_node(m, *n)),
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::IdentifierExpr { name } => json!({
            "$t": "IdentifierExpr",
            "name": name.as_str().to_string(),
        }),
        Syntax::ImportExpr { module } => json!({
            "$t": "ImportExpr",
           "module": visit_node(m, *module),
        }),
        Syntax::LiteralArrayExpr { elements } => json!({
            "$t": "LiteralArrayExpr",
            "elements": elements.iter().map(|e| match e {
                ArrayElement::Single(e) => json!({
                    "single": visit_node(m, *e),
                }),
                ArrayElement::Rest(e) => json!({
                    "rest": visit_node(m, *e),
                }),
                ArrayElement::Empty => Value::Null,
            }).collect::<Vec<_>>(),
        }),
        Syntax::LiteralBooleanExpr { value } => json!({
            "$t": "LiteralBooleanExpr",
            "value": value,
        }),
        Syntax::LiteralNull {} => json!({
            "$t": "LiteralNull",
        }),
        Syntax::LiteralNumberExpr { value } => json!({
            "$t": "LiteralNumberExpr",
            "value": value.0,
        }),
        Syntax::LiteralObjectExpr { members } => json!({
            "$t": "LiteralObjectExpr",
            "members": members.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::LiteralRegexExpr {} => json!({
            "$t": "LiteralRegexExpr",
        }),
        Syntax::LiteralStringExpr { value } => json!({
            "$t": "LiteralStringExpr",
            "value": value.as_str().to_string(),
        }),
        Syntax::LiteralTemplateExpr { parts } => json!({
            "$t": "LiteralTemplateExpr",
            "parts": parts.iter().map(|p| match p {
              LiteralTemplatePart::String(string) => json!({
                  "string": string.as_str().to_string(),
              }),
              LiteralTemplatePart::Substitution(sub) => json!({
                  "substitution": visit_node(m, *sub),
              }),
            }).collect::<Vec<_>>(),
        }),
        Syntax::LiteralUndefined {} => json!({
            "$t": "LiteralUndefined",
        }),
        Syntax::ThisExpr {} => json!({
            "$t": "ThisExpr",
        }),
        Syntax::UnaryExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": visit_node(m, *argument),
        }),
        Syntax::UnaryPostfixExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryPostfixExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": visit_node(m, *argument),
        }),
        Syntax::BlockStmt { body } => json!({
            "$t": "BlockStmt",
            "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::BreakStmt { label } => json!({
            "$t": "BreakStmt",
            "label": label.as_ref().map(|n| n.as_str().to_string()),
        }),
        Syntax::ContinueStmt { label } => json!({
            "$t": "ContinueStmt",
            "label": label.as_ref().map(|n| n.as_str().to_string()),
        }),
        Syntax::DebuggerStmt {} => json!({
            "$t": "DebuggerStmt",
        }),
        Syntax::DoWhileStmt { condition, body } => json!({
            "$t": "DoWhileStmt",
            "condition": visit_node(m, *condition),
            "body": visit_node(m, *body)
        }),
        Syntax::EmptyStmt {} => json!({
            "$t": "EmptyStmt",
        }),
        Syntax::ExportDeclStmt {
            declaration,
            default,
        } => json!({
            "$t": "ExportDeclStmt",
            "declaration": visit_node(m, *declaration),
            "default": default,
        }),
        Syntax::ExportDefaultExprStmt { expression } => json!({
            "$t": "ExportDefaultStmt",
            "expression": visit_node(m, *expression),
        }),
        Syntax::ExportListStmt { names, from } => todo!(),
        Syntax::ExpressionStmt { expression } => json!({
            "$t": "ExpressionStmt",
            "expression": visit_node(m, *expression),
        }),
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "IfStmt",
            "test": visit_node(m, *test),
            "consequent": visit_node(m, *consequent),
            "alternate": alternate.map(|n| visit_node(m, n)),
        }),
        Syntax::ImportStmt {
            default,
            names,
            module,
        } => todo!(),
        Syntax::ForStmt { header, body } => json!({
            "$t": "ForStmt",
            "header": match header {
                ForStmtHeader::Three { init, condition, post } => json!({
                    "three": json!({
                        "init": match init {
                            ForThreeInit::None => Value::Null,
                            ForThreeInit::Expression(e) => json!({
                                "expression": visit_node(m, *e),
                            }),
                            ForThreeInit::Declaration(e) => json!({
                                "declaration": visit_node(m, *e),
                            }),
                        },
                        "condition": condition.map(|n| visit_node(m, n)),
                        "post": post.map(|n| visit_node(m, n)),
                    }),
                }),
                ForStmtHeader::InOf { of, lhs, rhs } => json!({
                    "inOf": json!({
                        "of": of,
                        "lhs": match lhs {
                            ForInOfStmtHeaderLhs::Declaration(decl) => json!({
                                "declaration": visit_node(m, *decl),
                            }),
                            ForInOfStmtHeaderLhs::Pattern(pat) => json!({
                                "pattern": visit_node(m, *pat),
                            }),
                        },
                        "rhs": visit_node(m, *rhs),
                    }),
                }),
            },
            "body": visit_node(m, *body),
        }),
        Syntax::LabelStmt { name, statement } => json!({
            "$t": "LabelStmt",
            "name": name.as_str().to_string(),
            "statement": visit_node(m, *statement),
        }),
        Syntax::ReturnStmt { value } => json!({
            "$t": "ReturnStmt",
            "value": value.map(|n| visit_node(m, n)) }),
        Syntax::SwitchStmt { test, branches } => json!({
            "$t": "SwitchStmt",
            "test": visit_node(m, *test),
            "branches": branches.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ThrowStmt { value } => json!({
            "$t": "ThrowStmt",
            "value": visit_node(m, *value),
        }),
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => json!({
            "$t": "TryStmt",
            "wrapped": visit_node(m, *wrapped),
            "catch": catch.map(|n| visit_node(m, n)),
            "finally": finally.map(|n| visit_node(m, n)),
        }),
        Syntax::VarStmt { declaration } => json!({
            "$t": "VarStmt", "declaration": visit_node(m, *declaration) }),
        Syntax::WhileStmt { condition, body } => json!({
            "$t": "WhileStmt",
            "condition": visit_node(m, *condition),
            "body": visit_node(m, *body),
        }),
        Syntax::TopLevel { body } => json!({
            "$t": "TopLevel",
            "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>() }),
        Syntax::CatchBlock { parameter, body } => json!({
            "$t": "CatchBlock",
            "parameter": parameter.map(|n| visit_node(m, n)),
            "body": visit_node(m, *body),
        }),
        Syntax::SwitchBranch { case, body } => json!({
            "$t": "SwitchBranch",
            "case": case.map(|n| visit_node(m, n)),
            "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ObjectMember { typ } => json!({
            "$t": "ObjectMember",
            "typ": match typ {
                ObjectMemberType::Rest { value } => json!({
                    "rest": visit_node(m, *value),
                }),
                ObjectMemberType::Shorthand { name } => json!({
                    "shorthand": json!(name.as_str().to_string()),
                }),
                ObjectMemberType::Valued { key, value } => json!({
                    "valued": json!({
                        "key": visit_class_or_object_member_key(m, key),
                        "value": visit_class_or_object_member_value(m, value),
                    }),
                }),
            },
        }),
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => json!({
            "$t": "ObjectPatternProperty",
            "key": visit_class_or_object_member_key(m, key),
            "target": target.map(|n| visit_node(m, n)),
            "default": default_value.map(|n| visit_node(m, n)),
        }),
        Syntax::MemberExpr {
            parenthesised,
            optional_chaining,
            left,
            right,
        } => json!({
            "$t": "MemberExpr",
            "parenthesised": parenthesised,
            "optional_chaining": optional_chaining,
            "left": visit_node(m, *left),
            "right": right.as_str().to_string(),
        }),
        Syntax::CallArg { spread, value } => json!({
            "$t": "CallArg",
            "spread": spread,
            "value": visit_node(m, *value),
        }),
        Syntax::SuperExpr {} => json!({
            "$t": "SuperExpr",
        }),
    }
}

pub fn serialise_ast(node_map: &NodeMap, top_level_node_id: NodeId) -> Value {
    visit_node(node_map, top_level_node_id)
}
