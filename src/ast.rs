use std::fmt;
use std::fmt::{Debug, Formatter};

use crate::error::{TsError, TsErrorType};
use crate::num::JsNumber;
use crate::operator::OperatorName;
use crate::source::SourceRange;

// These are for readability only, and do not increase type safety or define different structures.
type Declaration = Node;
type Expression = Node;
type Pattern = Node;
type Signature = Node;
type Statement = Node;

#[derive(Eq, PartialEq, Debug)]
pub enum VarDeclMode {
    Const,
    Let,
    Var,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ArrayElement {
    Single(Expression),
    Rest(Expression),
    Empty,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ClassOrObjectMemberKey {
    Direct(SourceRange),
    Computed(Expression),
}

#[derive(Eq, PartialEq, Debug)]
pub enum ClassOrObjectMemberValue {
    Getter {
        body: Statement,
    },
    Method {
        signature: Signature,
        body: Statement,
    },
    Property {
        // Must be Some if object.
        initializer: Option<Expression>,
    },
    Setter {
        body: Statement,
        parameter: SourceRange,
    },
}

#[derive(Eq, PartialEq, Debug)]
pub struct ClassMember {
    pub key: ClassOrObjectMemberKey,
    pub statik: bool,
    pub value: ClassOrObjectMemberValue,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ObjectMember {
    Single {
        key: ClassOrObjectMemberKey,
        value: ClassOrObjectMemberValue,
    },
    SingleShorthand(SourceRange),
    Rest(Expression),
}

#[derive(Eq, PartialEq, Debug)]
pub struct ArrayPatternElement {
    pub target: Pattern,
    pub default_value: Option<Expression>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ObjectPatternProperty {
    pub key: ClassOrObjectMemberKey,
    // Omitted if shorthand i.e. key is Direct and target is IdentifierPattern of same name.
    pub target: Option<Pattern>,
    pub default_value: Option<Expression>,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ImportOrExportName {
    Default,
    Name(SourceRange),
}

#[derive(Eq, PartialEq, Debug)]
pub enum ImportNames {
    // `import * as name`.
    All(SourceRange),
    // `import {a as b, c, default as e}`.
    Specific(Vec<(ImportOrExportName, Option<SourceRange>)>),
}

#[derive(Eq, PartialEq, Debug)]
pub enum ExportNames {
    // `export * from "module"`.
    All,
    // `export {a as default, b as c, d}`, `export {default, a as b, c} from "module"`.
    Specific(Vec<(ImportOrExportName, Option<ImportOrExportName>)>),
}

#[derive(Eq, PartialEq, Debug)]
pub struct VariableDeclarator {
    pub pattern: Pattern,
    pub initializer: Option<Expression>,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ForThreeInit {
    None,
    Expression(Expression),
    Declaration(Declaration),
}

#[derive(Eq, PartialEq, Debug)]
pub enum ForInOfStmtHeaderLhs {
    Declaration(Declaration),
    Pattern(Pattern),
}

#[derive(Eq, PartialEq, Debug)]
pub struct SwitchBranch {
    // If None, it's `default`.
    pub case: Option<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TryCatch {
    pub parameter: Option<SourceRange>,
    pub body: Statement,
}

#[derive(Eq, PartialEq, Debug)]
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

#[derive(Eq, PartialEq, Debug)]
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
        properties: Vec<ObjectPatternProperty>,
        rest: Option<SourceRange>,
    },

    // Signatures.
    FunctionSignature {
        parameters: Vec<Declaration>,
    },

    // Declarations.
    ClassDecl {
        name: Option<SourceRange>,
        super_class: Option<SourceRange>,
        members: Vec<ClassMember>,
    },
    FunctionDecl {
        name: SourceRange,
        signature: Signature,
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
        signature: Signature,
        body: Node,
    },
    BinaryExpr {
        parenthesised: bool,
        operator: OperatorName,
        left: Expression,
        right: Expression,
    },
    CallExpr {
        parenthesised: bool,
        callee: Expression,
        arguments: Vec<Expression>,
    },
    ConditionalExpr {
        parenthesised: bool,
        test: Expression,
        consequent: Expression,
        alternate: Expression,
    },
    ComputedMemberExpr {
        object: Expression,
        member: Expression,
    },
    FunctionExpr {
        parenthesised: bool,
        name: Option<SourceRange>,
        signature: Signature,
        body: Statement,
    },
    IdentifierExpr {
        name: SourceRange,
    },
    // TODO Non-literal-string imports.
    ImportExpr {
        module: String,
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
        members: Vec<ObjectMember>,
    },
    LiteralRegexExpr {},
    LiteralStringExpr {
        value: String,
    },
    LiteralUndefined {},
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
    YieldExpr {
        argument: Expression,
        delegate: bool,
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
    },
    ExportDefaultStmt {
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
        default: Option<SourceRange>,
        names: Option<ImportNames>,
        module: String,
    },
    ForStmt {
        header: ForStmtHeader,
        body: Statement,
    },
    ReturnStmt {
        value: Option<Expression>,
    },
    SwitchStmt {
        test: Expression,
        branches: Vec<SwitchBranch>,
    },
    ThrowStmt {
        value: Expression,
    },
    TryStmt {
        wrapped: Statement,
        // One of these must be present.
        catch: Option<TryCatch>,
        finally: Option<Statement>,
    },
    VarStmt {
        declaration: Declaration,
    },
    WhileStmt {
        condition: Expression,
        body: Statement,
    },

    // Top level.
    TopLevel {
        body: Vec<Statement>,
    },
}

#[derive(Debug)]
struct NodeData {
    loc: SourceRange,
    stx: Syntax,
}

pub struct Node {
    data: Box<NodeData>,
}

impl Node {
    pub fn new(loc: SourceRange, stx: Syntax) -> Node {
        Node {
            data: Box::new(NodeData { loc, stx }),
        }
    }

    pub fn error(&self, typ: TsErrorType) -> TsError {
        TsError::from_loc(self.loc(), typ)
    }

    pub fn loc(&self) -> &SourceRange {
        &self.data.loc
    }

    pub fn stx(&self) -> &Syntax {
        &self.data.stx
    }

    pub fn stx_mut(&mut self) -> &mut Syntax {
        &mut self.data.stx
    }

    pub fn stx_take(self) -> Syntax {
        self.data.stx
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.stx() == other.stx()
    }
}

impl Eq for Node {}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.data.stx))
    }
}
