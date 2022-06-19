use std::collections::HashMap;
use std::rc::Rc;

use crate::{ast::Node, source::SourceRange};

pub struct Identifier {
    name: SourceRange,
}

struct SymbolData {
    name: SourceRange,
    declarator_pattern: Node,
}

#[derive(Clone)]
pub struct Symbol {
    data: Rc<SymbolData>,
}

#[derive(Clone)]
pub enum ScopeType {
    Module,
    Function,
    Block,
}

struct ScopeData {
    symbols: HashMap<Identifier, Symbol>,
    ancestor_function_or_toplevel: Scope,
    parent: Option<Scope>,
    typ: ScopeType,
}

#[derive(Clone)]
pub struct Scope {
    data: Rc<ScopeData>,
}

impl Scope {
    pub fn new(
        ancestor_function_or_toplevel: Scope,
        parent: Option<Scope>,
        typ: ScopeType,
    ) -> Scope {
        Scope {
            data: Rc::new(ScopeData {
                symbols: HashMap::new(),
                ancestor_function_or_toplevel,
                parent,
                typ,
            }),
        }
    }
}
