use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};

#[cfg(test)]
use serde::Serialize;

use crate::error::{SyntaxError, SyntaxErrorType};
use crate::num::JsNumber;
use crate::operator::OperatorName;
use crate::source::SourceRange;
use crate::symbol::ScopeId;

pub struct NodeData {
    loc: SourceRange,
    stx: Syntax,
    // For the purposes of disambiguation, the scope of a function or block is only set on its children and not itself. This is merely an arbitrary decision. For example, the scope created by a function is assigned to its signature nodes (and descendants e.g. default values), but not to the FunctionStmt itself. For a `for` loop, the scope created by it is assigned to its header nodes and descendants, but not to the ForStmt itself. For a block statement, the scope created by it is assigned to statements inside it, but not to the BlockStmt itself.
    scope: ScopeId,
}

impl NodeData {
    pub fn new(scope: ScopeId, loc: SourceRange, stx: Syntax) -> NodeData {
        NodeData {
            loc,
            stx,
            scope: scope.clone(),
        }
    }

    pub fn error(&self, typ: SyntaxErrorType) -> SyntaxError {
        SyntaxError::from_loc(self.loc(), typ, None)
    }

    pub fn loc(&self) -> &SourceRange {
        &self.loc
    }

    pub fn stx(&self) -> &Syntax {
        &self.stx
    }

    pub fn stx_mut(&mut self) -> &mut Syntax {
        &mut self.stx
    }

    #[allow(dead_code)]
    pub fn stx_take(self) -> Syntax {
        self.stx
    }

    pub fn scope(&self) -> ScopeId {
        self.scope
    }
}

impl Debug for NodeData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.stx))
    }
}

// To prevent ambiguity and confusion, don't derive Eq, as two nodes could be structurally equal even if they are different nodes.
#[derive(Debug, Clone, Copy)]
pub struct NodeId(usize);

impl NodeId {
    pub fn new(id: usize) -> NodeId {
        NodeId(id)
    }

    pub fn id(&self) -> usize {
        self.0
    }
}

pub struct NodeMap {
    nodes: Vec<NodeData>,
}

impl NodeMap {
    pub fn new() -> NodeMap {
        NodeMap { nodes: Vec::new() }
    }

    pub fn create_node(&mut self, scope: ScopeId, loc: SourceRange, stx: Syntax) -> NodeId {
        let id = self.nodes.len();
        self.nodes.push(NodeData::new(scope, loc, stx));
        NodeId::new(id)
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn push(&mut self, n: NodeData) -> () {
        self.nodes.push(n);
    }
}

impl Index<NodeId> for NodeMap {
    type Output = NodeData;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.0]
    }
}

impl IndexMut<NodeId> for NodeMap {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}

// These are for readability only, and do not increase type safety or define different structures.
type Declaration = NodeId;
type Expression = NodeId;
type Pattern = NodeId;
type Statement = NodeId;

#[derive(Eq, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum VarDeclMode {
    Const,
    Let,
    Var,
}

#[derive(Debug)]
pub enum ArrayElement {
    Single(Expression),
    Rest(Expression),
    Empty,
}

#[derive(Clone, Debug)]
pub enum ClassOrObjectMemberKey {
    // Identifier, keyword, string, or number.
    Direct(SourceRange),
    Computed(Expression),
}

#[derive(Debug)]
pub enum ClassOrObjectMemberValue {
    Getter {
        body: Statement,
    },
    Method {
        is_async: bool,
        generator: bool,
        signature: NodeId,
        body: Statement,
    },
    Property {
        // Must be Some if object, as shorthands are covered by ObjectMemberType::Shorthand (and are initialised).
        initializer: Option<Expression>,
    },
    Setter {
        body: Statement,
        parameter: Pattern,
    },
}

#[derive(Debug)]
pub struct ClassMember {
    pub key: ClassOrObjectMemberKey,
    pub statik: bool,
    pub value: ClassOrObjectMemberValue,
}

#[derive(Debug)]
pub enum ObjectMemberType {
    Valued {
        key: ClassOrObjectMemberKey,
        value: ClassOrObjectMemberValue,
    },
    Shorthand {
        name: SourceRange,
    },
    Rest {
        value: Expression,
    },
}

#[derive(Debug)]
pub struct ArrayPatternElement {
    pub target: Pattern,
    pub default_value: Option<Expression>,
}

#[derive(Debug)]
pub struct ExportName {
    // For simplicity, we always set both fields; for shorthands, both nodes are identical.
    pub target: SourceRange,
    // IdentifierPattern.
    pub alias: Pattern,
}

#[derive(Debug)]
pub enum ExportNames {
    // `import * as name`
    // `export * from "module"`
    // `export * as name from "module"`
    // IdentifierPattern.
    All(Option<Pattern>),
    // `import {a as b, c, default as e}`
    // `export {a as default, b as c, d}`
    // `export {default, a as b, c} from "module"`
    // `default` is still a name, so we don't use an enum.
    Specific(Vec<ExportName>),
}

#[derive(Debug)]
pub struct VariableDeclarator {
    pub pattern: Pattern,
    pub initializer: Option<Expression>,
}

#[derive(Debug)]
pub enum ForThreeInit {
    None,
    Expression(Expression),
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum ForInOfStmtHeaderLhs {
    Declaration(Declaration),
    Pattern(Pattern),
}

#[derive(Debug)]
pub enum ForStmtHeader {
    Three {
        init: ForThreeInit,
        condition: Option<Expression>,
        post: Option<Expression>,
    },
    InOf {
        of: bool,
        lhs: ForInOfStmtHeaderLhs,
        rhs: Expression,
    },
}

#[derive(Debug)]
pub enum LiteralTemplatePart {
    Substitution(Expression),
    String(SourceRange),
}

// We no longer derive Eq for the AST due to use of NodeId, as it's not possible to determine structural equality without the node map. Anything that contains a NodeId/Syntax must also avoid Eq.
#[derive(Debug)]
pub enum Syntax {
    // Patterns.
    IdentifierPattern {
        name: SourceRange,
    },
    // `const fn = (a: any, b: any, ...{ length, ...c }: any[]) => void 0` is allowed.
    ArrayPattern {
        // Unnamed elements can exist.
        elements: Vec<Option<ArrayPatternElement>>,
        rest: Option<Pattern>,
    },
    // For an object pattern, `...` must be followed by an identifier.
    // `const fn = ({ a: { b = c } = d, ...e }: any) => void 0` is possible.
    ObjectPattern {
        // List of ObjectPatternProperty nodes.
        properties: Vec<NodeId>,
        // This must be IdentifierPattern, anything else is illegal.
        rest: Option<Pattern>,
    },
    // Not really a pattern but functions similarly; separated out for easy replacement when minifying.
    ClassOrFunctionName {
        name: SourceRange,
    },

    // Signatures.
    FunctionSignature {
        parameters: Vec<Declaration>,
    },

    // Declarations.
    ClassDecl {
        name: Option<NodeId>, // Name can only be omitted in a default export.
        extends: Option<Expression>,
        members: Vec<ClassMember>,
    },
    FunctionDecl {
        generator: bool,
        is_async: bool,
        name: Option<NodeId>, // Name can only be omitted in a default export.
        signature: NodeId,
        body: Statement,
    },
    ParamDecl {
        rest: bool,
        pattern: Pattern,
        default_value: Option<Expression>,
    },
    VarDecl {
        mode: VarDeclMode,
        declarators: Vec<VariableDeclarator>,
    },

    // Expressions.
    ArrowFunctionExpr {
        is_async: bool,
        signature: NodeId,
        body: NodeId,
    },
    BinaryExpr {
        parenthesised: bool,
        operator: OperatorName,
        left: Expression,
        right: Expression,
    },
    CallExpr {
        optional_chaining: bool,
        parenthesised: bool,
        callee: Expression,
        arguments: Vec<NodeId>,
    },
    ClassExpr {
        parenthesised: bool,
        name: Option<NodeId>,
        extends: Option<Expression>,
        members: Vec<ClassMember>,
    },
    ConditionalExpr {
        parenthesised: bool,
        test: Expression,
        consequent: Expression,
        alternate: Expression,
    },
    ComputedMemberExpr {
        optional_chaining: bool,
        object: Expression,
        member: Expression,
    },
    FunctionExpr {
        parenthesised: bool,
        is_async: bool,
        generator: bool,
        name: Option<NodeId>,
        signature: NodeId,
        body: Statement,
    },
    IdentifierExpr {
        name: SourceRange,
    },
    ImportExpr {
        module: Expression,
    },
    LiteralArrayExpr {
        elements: Vec<ArrayElement>,
    },
    LiteralBooleanExpr {
        value: bool,
    },
    LiteralNull {},
    LiteralNumberExpr {
        value: JsNumber,
    },
    LiteralObjectExpr {
        // List of ObjectMember nodes.
        members: Vec<NodeId>,
    },
    LiteralRegexExpr {},
    LiteralStringExpr {
        value: String,
    },
    LiteralTemplateExpr {
        parts: Vec<LiteralTemplatePart>,
    },
    LiteralUndefined {},
    // Dedicated special type to easily distinguish when analysing and minifying. Also done to avoid using IdentifierExpr as right, which is incorrect (not a variable usage).
    MemberExpr {
        parenthesised: bool,
        optional_chaining: bool,
        left: Expression,
        right: SourceRange,
    },
    SuperExpr {},
    ThisExpr {},
    UnaryExpr {
        parenthesised: bool,
        operator: OperatorName,
        argument: Expression,
    },
    UnaryPostfixExpr {
        parenthesised: bool,
        operator: OperatorName,
        argument: Expression,
    },

    // Statements.
    BlockStmt {
        body: Vec<Statement>,
    },
    BreakStmt {
        label: Option<SourceRange>,
    },
    ContinueStmt {
        label: Option<SourceRange>,
    },
    DebuggerStmt {},
    DoWhileStmt {
        condition: Expression,
        body: Statement,
    },
    EmptyStmt {},
    ExportDeclStmt {
        declaration: Declaration,
        default: bool,
    },
    ExportDefaultExprStmt {
        expression: Expression,
    },
    ExportListStmt {
        names: ExportNames,
        from: Option<String>,
    },
    ExpressionStmt {
        expression: Expression,
    },
    IfStmt {
        test: Expression,
        consequent: Statement,
        alternate: Option<Statement>,
    },
    ImportStmt {
        // IdentifierPattern.
        default: Option<Pattern>,
        names: Option<ExportNames>,
        module: String,
    },
    ForStmt {
        header: ForStmtHeader,
        body: Statement,
    },
    LabelStmt {
        name: SourceRange,
        statement: Statement,
    },
    ReturnStmt {
        value: Option<Expression>,
    },
    SwitchStmt {
        test: Expression,
        branches: Vec<NodeId>,
    },
    ThrowStmt {
        value: Expression,
    },
    TryStmt {
        wrapped: Statement,
        // One of these must be present.
        catch: Option<NodeId>,
        finally: Option<Statement>,
    },
    VarStmt {
        declaration: Declaration,
    },
    WhileStmt {
        condition: Expression,
        body: Statement,
    },

    // Others.
    TopLevel {
        body: Vec<Statement>,
    },
    CallArg {
        spread: bool,
        value: Expression,
    },
    CatchBlock {
        parameter: Option<Pattern>,
        body: Statement,
    },
    // This is a node instead of an enum so that we can replace it when minifying e.g. expanding shorthand to `key: value`.
    ObjectMember {
        typ: ObjectMemberType,
    },
    ObjectPatternProperty {
        key: ClassOrObjectMemberKey,
        // Omitted if shorthand i.e. key is Direct and target is IdentifierPattern of same name.
        // TODO Ideally for simplicity this should be duplicated from `key` if shorthand, with a `shorthand` boolean field to indicate so.
        target: Option<Pattern>,
        default_value: Option<Expression>,
    },
    SwitchBranch {
        // If None, it's `default`.
        case: Option<Expression>,
        body: Vec<Statement>,
    },
}
