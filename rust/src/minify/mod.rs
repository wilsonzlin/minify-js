use crate::{
    ast::{
        ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ExportName, ExportNames,
        ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, LiteralTemplatePart, NodeId, NodeMap,
        ObjectMemberType, Syntax,
    },
    char::{ID_CONTINUE_CHARSTR, ID_START_CHARSTR},
    lex::KEYWORD_STRS,
    source::SourceRange,
    symbol::{ScopeId, ScopeMap},
    update::NodeUpdates,
};

const ALT_MINIFIED_NAMES: &'static [char] = &[
    '\u{01BB}', '\u{02B0}', '\u{02B1}', '\u{02B2}', '\u{02B3}', '\u{02B4}', '\u{02B5}', '\u{02B6}',
    '\u{02B7}', '\u{02B8}', '\u{02B9}', '\u{02BA}', '\u{02BB}', '\u{02BC}', '\u{02BD}', '\u{02BE}',
    '\u{02BF}', '\u{02C0}', '\u{02C1}', '\u{02C6}', '\u{02C7}', '\u{02C8}', '\u{02C9}', '\u{02CA}',
    '\u{02CB}', '\u{02CC}', '\u{02CD}', '\u{02CE}', '\u{02CF}', '\u{02D0}', '\u{02D1}', '\u{02E0}',
    '\u{02E1}', '\u{02E2}', '\u{02E3}', '\u{02E4}', '\u{02EC}', '\u{02EE}', '\u{0374}', '\u{037A}',
    '\u{0559}', '\u{0640}', '\u{06E5}', '\u{06E6}', '\u{07F4}', '\u{07F5}', '\u{07FA}',
];

fn generate_minified_name(mut id: usize) -> SourceRange {
    let mut name = vec![ID_START_CHARSTR[id % ID_START_CHARSTR.len()]];
    if id >= ID_START_CHARSTR.len() {
        id /= ID_START_CHARSTR.len();
        while id >= ID_CONTINUE_CHARSTR.len() {
            name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
            id /= ID_CONTINUE_CHARSTR.len();
        }
        name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
    };
    if let Some(alt_id) = KEYWORD_STRS.get(name.as_slice()) {
        // This name is a keyword, so we replace it with a Unicode character instead.
        // This Unicode character is 2 bytes when encoded in UTF-8, so it's more than minimal enough. UTF-8 encodes U+0080 to U+07FF in 2 bytes.
        // There should be exactly one ALT_MINIFIED_NAMES element for each KEYWORD_STRS entry.
        // Using a Unicode name will ensure no chance of clashing with keywords, well-knowns, and almost all variables.
        // Clashes can appear quickly e.g. `in`, `of`, `if`.
        let s = ALT_MINIFIED_NAMES[*alt_id].encode_utf8(&mut name).len();
        name.truncate(s);
    };
    SourceRange::anonymous(name)
}

struct ExportBinding {
    target: SourceRange,
    alias: SourceRange,
}

struct VisitorCtx<'a> {
    scopes: &'a ScopeMap,
    nodes: &'a NodeMap,
    updates: &'a mut NodeUpdates,
    // Exports with the same exported name (including multiple default exports) are illegal, so we don't have to worry about/handle that case.
    export_bindings: &'a mut Vec<ExportBinding>,
}

fn visit_class_or_object_key(ctx: &mut VisitorCtx, key: &ClassOrObjectMemberKey) -> () {
    match key {
        ClassOrObjectMemberKey::Direct(_) => {}
        ClassOrObjectMemberKey::Computed(e) => visit_node(ctx, *e),
    };
}

fn visit_class_or_object_value(ctx: &mut VisitorCtx, value: &ClassOrObjectMemberValue) -> () {
    match value {
        ClassOrObjectMemberValue::Getter { body } => visit_node(ctx, *body),
        ClassOrObjectMemberValue::Method {
            signature, body, ..
        } => {
            visit_node(ctx, *signature);
            visit_node(ctx, *body);
        }
        ClassOrObjectMemberValue::Property { initializer } => {
            if let Some(initializer) = initializer {
                visit_node(ctx, *initializer);
            };
        }
        ClassOrObjectMemberValue::Setter { body, parameter } => {
            visit_node(ctx, *parameter);
            visit_node(ctx, *body);
        }
    }
}

fn visit_exported_pattern(ctx: &mut VisitorCtx, n: NodeId) -> () {
    match ctx.nodes[n].stx() {
        Syntax::ArrayPattern { elements, rest } => {
            for e in elements {
                if let Some(e) = e {
                    visit_exported_pattern(ctx, e.target);
                }
            }
            if let Some(rest) = rest {
                visit_exported_pattern(ctx, *rest);
            }
        }
        Syntax::ObjectPattern { properties, rest } => {
            for p in properties {
                visit_exported_pattern(ctx, *p);
            }
            if let Some(rest) = rest {
                visit_exported_pattern(ctx, *rest);
            }
        }
        Syntax::ObjectPatternProperty { key, target, .. } => {
            match target {
                Some(target) => visit_exported_pattern(ctx, *target),
                // Shorthand.
                None => match key {
                    ClassOrObjectMemberKey::Direct(key) => {
                        ctx.export_bindings.push(ExportBinding {
                            target: key.clone(),
                            alias: key.clone(),
                        })
                    }
                    _ => unreachable!(),
                },
            }
        }
        Syntax::IdentifierPattern { name } => ctx.export_bindings.push(ExportBinding {
            target: name.clone(),
            alias: name.clone(),
        }),
        _ => unreachable!(),
    }
}

fn visit_node(ctx: &mut VisitorCtx, n: NodeId) -> () {
    let scope_id = ctx.nodes[n].scope();
    let scope = &ctx.scopes[scope_id];
    match ctx.nodes[n].stx() {
        Syntax::FunctionExpr {
            name,
            signature,
            body,
            ..
        } => {
            if let Some(name) = name {
                visit_node(ctx, *name);
            };
            visit_node(ctx, *signature);
            visit_node(ctx, *body);
        }
        stx @ (Syntax::IdentifierPattern { name }
        | Syntax::IdentifierExpr { name }
        | Syntax::ClassOrFunctionName { name }) => {
            let sym = scope.find_symbol(ctx.scopes, name);
            if let Some(sym) = sym {
                let minified = generate_minified_name(sym.minified_name_id());
                ctx.updates.replace_node(
                    n,
                    scope_id,
                    minified.clone(),
                    match stx {
                        Syntax::IdentifierPattern { .. } => Syntax::IdentifierPattern {
                            name: minified.clone(),
                        },
                        Syntax::IdentifierExpr { .. } => Syntax::IdentifierExpr {
                            name: minified.clone(),
                        },
                        Syntax::ClassOrFunctionName { .. } => Syntax::ClassOrFunctionName {
                            name: minified.clone(),
                        },
                        _ => unreachable!(),
                    },
                );
            };
        }
        Syntax::ArrayPattern { elements, rest } => {
            for e in elements {
                if let Some(e) = e {
                    visit_node(ctx, e.target);
                };
            }
            if let Some(r) = rest {
                visit_node(ctx, *r);
            };
        }
        Syntax::ArrowFunctionExpr {
            signature, body, ..
        } => {
            visit_node(ctx, *signature);
            visit_node(ctx, *body);
        }
        Syntax::BinaryExpr { left, right, .. } => {
            visit_node(ctx, *left);
            visit_node(ctx, *right);
        }
        Syntax::BlockStmt { body } => {
            for stmt in body {
                visit_node(ctx, *stmt);
            }
        }
        Syntax::BreakStmt { .. } => {}
        Syntax::CallExpr {
            callee, arguments, ..
        } => {
            visit_node(ctx, *callee);
            for arg in arguments {
                visit_node(ctx, *arg);
            }
        }
        Syntax::CatchBlock { parameter, body } => {
            if let Some(p) = parameter {
                visit_node(ctx, *p);
            }
            visit_node(ctx, *body);
        }
        Syntax::ClassDecl {
            name,
            extends,
            members,
        } => {
            visit_node(ctx, *name);
            if let Some(extends) = extends {
                visit_node(ctx, *extends);
            };
            for member in members {
                visit_class_or_object_key(ctx, &member.key);
                visit_class_or_object_value(ctx, &member.value);
            }
        }
        Syntax::ClassExpr {
            name,
            extends,
            members,
            ..
        } => {
            if let Some(name) = name {
                visit_node(ctx, *name);
            }
            if let Some(extends) = extends {
                visit_node(ctx, *extends);
            };
            for member in members {
                visit_class_or_object_key(ctx, &member.key);
                visit_class_or_object_value(ctx, &member.value);
            }
        }
        Syntax::ComputedMemberExpr { object, member, .. } => {
            visit_node(ctx, *object);
            visit_node(ctx, *member);
        }
        Syntax::ConditionalExpr {
            test,
            consequent,
            alternate,
            ..
        } => {
            visit_node(ctx, *test);
            visit_node(ctx, *consequent);
            visit_node(ctx, *alternate);
        }
        Syntax::ContinueStmt { .. } => {}
        Syntax::DebuggerStmt {} => {}
        Syntax::DoWhileStmt { condition, body } => {
            visit_node(ctx, *body);
            visit_node(ctx, *condition);
        }
        Syntax::EmptyStmt {} => {}
        Syntax::ExportDeclStmt { declaration } => {
            match ctx.nodes[*declaration].stx() {
                Syntax::ClassDecl { name, .. } | Syntax::FunctionDecl { name, .. } => {
                    match ctx.nodes[*name].stx() {
                        Syntax::ClassOrFunctionName { name } => {
                            ctx.export_bindings.push(ExportBinding {
                                target: name.clone(),
                                alias: name.clone(),
                            });
                        }
                        _ => unreachable!(),
                    }
                }
                Syntax::VarStmt { declaration } => match ctx.nodes[*declaration].stx() {
                    Syntax::VarDecl { declarators, .. } => {
                        for decl in declarators {
                            visit_exported_pattern(ctx, decl.pattern);
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
            visit_node(ctx, *declaration);
        }
        Syntax::ExportDefaultExprStmt { expression } => visit_node(ctx, *expression),
        Syntax::ExportListStmt { names, from } => match from {
            // `export ... from ...` do not touch/alter the module's scope, so we can ignore completely.
            Some(_) => {}
            None => match names {
                ExportNames::Specific(names) => {
                    for e in names {
                        ctx.export_bindings.push(ExportBinding {
                            target: e.target.clone(),
                            alias: match ctx.nodes[e.alias].stx() {
                                Syntax::IdentifierPattern { name } => name.clone(),
                                _ => unreachable!(),
                            },
                        });
                        ctx.updates.replace_node(
                            n,
                            scope_id,
                            SourceRange::anonymous(""),
                            Syntax::EmptyStmt {},
                        );
                    }
                }
                ExportNames::All(_) => unreachable!(),
            },
        },
        Syntax::ExpressionStmt { expression } => {
            visit_node(ctx, *expression);
        }
        Syntax::ForStmt { header, body } => {
            match header {
                ForStmtHeader::Three {
                    init,
                    condition,
                    post,
                } => {
                    match init {
                        ForThreeInit::None => {}
                        ForThreeInit::Expression(n) => visit_node(ctx, *n),
                        ForThreeInit::Declaration(n) => visit_node(ctx, *n),
                    };
                    if let Some(condition) = condition {
                        visit_node(ctx, *condition);
                    }
                    if let Some(post) = post {
                        visit_node(ctx, *post);
                    }
                }
                ForStmtHeader::InOf { lhs, rhs, .. } => {
                    match lhs {
                        ForInOfStmtHeaderLhs::Declaration(n) => visit_node(ctx, *n),
                        ForInOfStmtHeaderLhs::Pattern(n) => visit_node(ctx, *n),
                    }
                    visit_node(ctx, *rhs);
                }
            };
            visit_node(ctx, *body);
        }
        Syntax::FunctionDecl {
            name,
            signature,
            body,
            ..
        } => {
            visit_node(ctx, *name);
            visit_node(ctx, *signature);
            visit_node(ctx, *body);
        }
        Syntax::FunctionSignature { parameters } => {
            for p in parameters {
                visit_node(ctx, *p);
            }
        }
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => {
            visit_node(ctx, *test);
            visit_node(ctx, *consequent);
            if let Some(alternate) = alternate {
                visit_node(ctx, *alternate);
            };
        }
        Syntax::ImportExpr { module } => visit_node(ctx, *module),
        Syntax::ImportStmt { default, names, .. } => {
            if let Some(default) = default {
                visit_node(ctx, *default);
            };
            for n in names {
                match n {
                    ExportNames::All(alias) => {
                        if let Some(alias) = alias {
                            visit_node(ctx, *alias);
                        }
                    }
                    ExportNames::Specific(names) => {
                        for n in names {
                            visit_node(ctx, n.alias);
                        }
                    }
                }
            }
        }
        Syntax::LiteralArrayExpr { elements } => {
            for e in elements {
                match e {
                    ArrayElement::Single(e) => visit_node(ctx, *e),
                    ArrayElement::Rest(e) => visit_node(ctx, *e),
                    ArrayElement::Empty => {}
                }
            }
        }
        Syntax::LiteralBooleanExpr { .. } => {}
        Syntax::LiteralNull {} => {}
        Syntax::LiteralNumberExpr { .. } => {}
        Syntax::LiteralObjectExpr { members } => {
            for member in members {
                visit_node(ctx, *member);
            }
        }
        Syntax::LiteralRegexExpr {} => {}
        Syntax::LiteralStringExpr { .. } => {}
        Syntax::LiteralTemplateExpr { parts } => {
            for p in parts {
                match p {
                    LiteralTemplatePart::Substitution(expr) => visit_node(ctx, *expr),
                    LiteralTemplatePart::String(_) => {}
                }
            }
        }
        Syntax::LiteralUndefined {} => {}
        Syntax::ObjectPattern { properties, rest } => {
            for p in properties {
                visit_node(ctx, *p);
            }
            if let Some(r) = rest {
                visit_node(ctx, *r);
            }
        }
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => {
            match key {
                ClassOrObjectMemberKey::Direct(name) => {
                    if target.is_none() {
                        let sym = scope.find_symbol(ctx.scopes, name);
                        if let Some(sym) = sym {
                            let minified = generate_minified_name(sym.minified_name_id());
                            let replacement_target_node = ctx.updates.create_node(
                                scope_id,
                                minified.clone(),
                                Syntax::IdentifierPattern {
                                    name: minified.clone(),
                                },
                            );
                            ctx.updates.replace_node(
                                n,
                                scope_id,
                                minified.clone(),
                                Syntax::ObjectPatternProperty {
                                    key: key.clone(),
                                    target: Some(replacement_target_node),
                                    default_value: default_value.clone(),
                                },
                            );
                        };
                    }
                }
                ClassOrObjectMemberKey::Computed(c) => visit_node(ctx, *c),
            };
            if let Some(target) = target {
                visit_node(ctx, *target);
            }
            if let Some(v) = default_value {
                visit_node(ctx, *v);
            }
        }
        Syntax::ParamDecl {
            pattern,
            default_value,
            ..
        } => {
            visit_node(ctx, *pattern);
            if let Some(v) = default_value {
                visit_node(ctx, *v);
            }
        }
        Syntax::ReturnStmt { value } => {
            if let Some(v) = value {
                visit_node(ctx, *v);
            }
        }
        Syntax::SwitchBranch { case, body } => {
            if let Some(v) = case {
                visit_node(ctx, *v);
            }
            for stmt in body {
                visit_node(ctx, *stmt);
            }
        }
        Syntax::SwitchStmt { test, branches } => {
            visit_node(ctx, *test);
            for b in branches {
                visit_node(ctx, *b);
            }
        }
        Syntax::ThisExpr {} => {}
        Syntax::ThrowStmt { value } => {
            visit_node(ctx, *value);
        }
        Syntax::TopLevel { body } => {
            for stmt in body {
                visit_node(ctx, *stmt);
            }
        }
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => {
            visit_node(ctx, *wrapped);
            if let Some(catch) = catch {
                visit_node(ctx, *catch);
            }
            if let Some(finally) = finally {
                visit_node(ctx, *finally);
            }
        }
        Syntax::UnaryExpr { argument, .. } => {
            visit_node(ctx, *argument);
        }
        Syntax::UnaryPostfixExpr { argument, .. } => {
            visit_node(ctx, *argument);
        }
        Syntax::VarDecl { declarators, .. } => {
            for decl in declarators {
                visit_node(ctx, decl.pattern);
                if let Some(e) = decl.initializer {
                    visit_node(ctx, e);
                }
            }
        }
        Syntax::VarStmt { declaration } => {
            visit_node(ctx, *declaration);
        }
        Syntax::WhileStmt { condition, body } => {
            visit_node(ctx, *condition);
            visit_node(ctx, *body);
        }
        Syntax::ObjectMember { typ } => {
            match typ {
                ObjectMemberType::Valued { key, value } => {
                    visit_class_or_object_key(ctx, key);
                    visit_class_or_object_value(ctx, value);
                }
                ObjectMemberType::Shorthand { name } => {
                    let sym = scope.find_symbol(ctx.scopes, name);
                    if let Some(sym) = sym {
                        let minified = generate_minified_name(sym.minified_name_id());
                        let replacement_initializer_node = ctx.updates.create_node(
                            scope_id,
                            minified.clone(),
                            Syntax::IdentifierExpr {
                                name: minified.clone(),
                            },
                        );
                        ctx.updates.replace_node(
                            n,
                            scope_id,
                            minified.clone(),
                            Syntax::ObjectMember {
                                typ: ObjectMemberType::Valued {
                                    key: ClassOrObjectMemberKey::Direct(name.clone()),
                                    value: ClassOrObjectMemberValue::Property {
                                        initializer: Some(replacement_initializer_node),
                                    },
                                },
                            },
                        );
                    };
                }
                ObjectMemberType::Rest { value } => {
                    visit_node(ctx, *value);
                }
            };
        }
        Syntax::MemberExpr { left, .. } => {
            visit_node(ctx, *left);
        }
        Syntax::LabelStmt { statement, .. } => {
            visit_node(ctx, *statement);
        }
        Syntax::CallArg { value, .. } => {
            visit_node(ctx, *value);
        }
        Syntax::SuperExpr {} => {}
    };
}

pub fn minify_js(
    scope_map: &mut ScopeMap,
    node_map: &mut NodeMap,
    top_level_scope_id: ScopeId,
    top_level_node_id: NodeId,
) -> () {
    // We need these since we cannot look up in scope_map while mutably borrowed by iter_mut().
    let symbol_counts: Vec<usize> = scope_map.iter().map(|s| s.symbol_count()).collect();
    let mut minified_name_starts: Vec<usize> = vec![0; scope_map.len()];
    // Iterating like this assumes that any parent of an element is always present before it in the list.
    for (id, scope) in scope_map.iter_mut().enumerate() {
        // TODO Opportunity for optimisation: not all variables are used by descendant scopes, so some of their names can be reused by descendants.
        let minified_name_start = match scope.parent() {
            Some(p) => symbol_counts[p.id()] + minified_name_starts[p.id()],
            None => 0,
        };
        scope.symbols_update(|no, symbol| {
            symbol.set_minified_name_id(minified_name_start + no);
        });
        minified_name_starts[id] = minified_name_start;
    }

    let mut export_bindings = Vec::new();
    let mut updates = NodeUpdates::new(node_map);
    let mut ctx = VisitorCtx {
        export_bindings: &mut export_bindings,
        nodes: node_map,
        scopes: scope_map,
        updates: &mut updates,
    };
    match node_map[top_level_node_id].stx() {
        Syntax::TopLevel { body } => {
            for n in body.iter() {
                visit_node(&mut ctx, *n);
            }
        }
        _ => panic!("not top level"),
    };
    updates.apply_updates(node_map);
    let export_names = export_bindings
        .iter()
        .map(|e| ExportName {
            target: generate_minified_name(
                scope_map[top_level_scope_id]
                    .find_symbol(scope_map, &e.target)
                    .expect(format!("failed to find top-level export `{:?}`", e.target).as_str())
                    .minified_name_id(),
            ),
            alias: node_map.create_node(
                top_level_scope_id,
                e.alias.clone(),
                Syntax::IdentifierPattern {
                    name: e.alias.clone(),
                },
            ),
        })
        .collect::<Vec<ExportName>>();

    if !export_names.is_empty() {
        let final_export_stmt = node_map.create_node(
            top_level_scope_id,
            SourceRange::anonymous(""),
            Syntax::ExportListStmt {
                names: ExportNames::Specific(export_names),
                from: None,
            },
        );
        match node_map[top_level_node_id].stx_mut() {
            Syntax::TopLevel { body } => {
                body.push(final_export_stmt);
            }
            _ => unreachable!(),
        }
    }
}
