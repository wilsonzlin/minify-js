use ahash::AHashMap;
use num_bigint::BigInt;
use parse_js::num::JsNumber;
use symbol_js::symbol::Symbol;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::fmt::{self};

// PartialOrd and Ord are for some arbitrary canonical order, even if semantics of ordering is opaque.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Const {
  BigInt(BigInt),
  Bool(bool),
  Null,
  Num(JsNumber),
  Str(String),
  Undefined,
}

impl Debug for Const {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::BigInt(v) => write!(f, "{v}"),
      Self::Bool(v) => write!(f, "{v}"),
      Self::Null => write!(f, "null"),
      Self::Num(v) => write!(f, "{v}"),
      Self::Str(v) => write!(f, "'{v}'"),
      Self::Undefined => write!(f, "undefined"),
    }
  }
}

// PartialOrd and Ord are for some arbitrary canonical order, even if semantics of ordering are opaque.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Arg {
  Builtin(String), // The value is a path (e.g. `Array.prototype.forEach`). Using a single string makes it easier to match.
  Const(Const),
  Var(u32),
}

impl Arg {
  pub fn maybe_var(&self) -> Option<u32> {
    match self {
      Arg::Var(n) => Some(*n),
      _ => None,
    }
  }

  pub fn to_var(&self) -> u32 {
    self
      .maybe_var()
      .expect("cannot convert constant to variable")
  }

  pub fn to_const(&self) -> Const {
    match self {
      Arg::Const(c) => c.clone(),
      _ => panic!("not a constant"),
    }
  }
}

impl Debug for Arg {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Builtin(p) => write!(f, "{p}"),
      Self::Const(v) => write!(f, "{v:?}"),
      Self::Var(n) => write!(f, "%{n}"),
    }
  }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum BinOp {
  Add,
  Div, // Divide.
  Exp, // Exponentiate.
  Geq, // Greater than or equals to.
  GetProp,
  Gt,  // Greater than.
  Leq, // Less than or equals to.
  LooseEq,
  Lt,  // Less than.
  Mod, // Modulo.
  Mul, // Multiply.
  NotLooseEq,
  NotStrictEq,
  StrictEq,
  Sub, // Subtract.
}

impl Debug for BinOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Add => write!(f, "+"),
      Self::Div => write!(f, "/"),
      Self::Exp => write!(f, "**"),
      Self::Geq => write!(f, ">="),
      Self::GetProp => write!(f, "."),
      Self::Gt => write!(f, ">"),
      Self::Leq => write!(f, "<="),
      Self::LooseEq => write!(f, "=="),
      Self::Lt => write!(f, "<"),
      Self::Mod => write!(f, "%"),
      Self::Mul => write!(f, "*"),
      Self::NotLooseEq => write!(f, "!="),
      Self::NotStrictEq => write!(f, "!=="),
      Self::StrictEq => write!(f, "==="),
      Self::Sub => write!(f, "-"),
    }
  }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum UnOp {
  Neg,
  Not,
  Plus,
  Typeof,
  Void,
}

impl Debug for UnOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Neg => write!(f, "-"),
      Self::Not => write!(f, "!"),
      Self::Plus => write!(f, "+"),
      Self::Typeof => write!(f, "typeof"),
      Self::Void => write!(f, "void"),
    }
  }
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum CallArg {
  Arg(Arg),
  Spread(Arg),
}

impl CallArg {
  pub fn as_arg(&self) -> &Arg {
    match self {
      CallArg::Arg(a) => a,
      CallArg::Spread(_) => unreachable!(),
    }
  }
}

#[derive(PartialEq, Eq)]
pub(crate) enum Inst {
  Bin {
    tgt: u32,
    left: Arg,
    op: BinOp,
    right: Arg,
  },
  Un {
    tgt: u32,
    arg: Arg,
    op: UnOp,
  },
  VarAssign {
    tgt: u32,
    value: Arg,
  },
  PropAssign {
    obj: Arg,
    prop: Arg,
    value: Arg,
  },
  Goto {
    label: u32,
  },
  CondGoto {
    cond: Arg,
    label: u32,
  },
  NotCondGoto {
    cond: Arg,
    label: u32,
  },
  Call {
    tgt: Option<u32>,
    func: Arg,
    this: Arg, // Can be `Arg::Const(Const::Undefined)`.
    args: Vec<CallArg>,
  },
  // A foreign variable is one in an ancestor scope, all the way up to and including the global scope.
  // We don't simply add another Target variant (e.g. Target::Foreign) as it makes analyses and optimisations more tedious. Consider that standard SSA doesn't really have a concept of nonlocal memory locations. In LLVM such vars are covered using ordinary memory location read/write instructions.
  // NOTE: It still violates SSA if we only have ForeignStore but not ForeignLoad (and instead use another enum variant for Arg). Consider: `%a0 = foreign(3); %a1 = %a0 + 42; foreign(3) = %a1; %a2 = foreign(3);` but `%a0` and `%a2` are not identical.
  ForeignLoad {
    from: Symbol,
    to: u32,
  },
  ForeignStore {
    from: Arg,
    to: Symbol,
  },
  // Same as Foreign* except we don't know the symbol.
  UnknownLoad {
    from: String,
    to: u32,
  },
  UnknownStore {
    from: Arg,
    to: String,
  },
  Phi {
    tgt: u32,
    from_blocks: AHashMap<u32, Arg>, // Pick one assigned value of `tgt` from one of these blocks. Due to const propagation, input targets could be transformed to const values, which is why we have `Arg` and not just `Target`.
  },
  // No-op marker for a position in Vec<Inst>. We can't just use indices as we may reorder and splice the instructions during optimisations.
  Label {
    label: u32,
  },
}

impl Debug for Inst {
  #[rustfmt::skip]
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Inst::Bin { tgt, left, op, right } => write!(f, "%{tgt} = {left:?} {op:?} {right:?}"),
      Inst::Call { tgt, func, this, args } => write!(f, "%{tgt:?} = call {func:?} with this={this:?} args={args:?}"),
      Inst::CondGoto { cond, label } => write!(f, "goto @{label} if {cond:?}"),
      Inst::ForeignLoad { from, to } => write!(f, "%{to} = foreign {from:?}"),
      Inst::ForeignStore { from, to } => write!(f, "foreign {to:?} = {from:?}"),
      Inst::Goto { label } => write!(f, "goto @{label}"),
      Inst::Label { label } => write!(f, "@{label}:"),
      Inst::NotCondGoto { cond, label } => write!(f, "goto @{label} ifnot {cond:?}"),
      Inst::Phi { tgt, from_blocks } => write!(f, "%{tgt} = ϕ{from_blocks:?}"),
      Inst::PropAssign { obj, prop, value } => write!(f, "{obj:?}[{prop:?}] = {value:?}"),
      Inst::Un { tgt, op, arg } => write!(f, "%{tgt} = {op:?} {arg:?}"),
      Inst::UnknownLoad { from, to } => write!(f, "%{to} = unknown {from:?}"),
      Inst::UnknownStore { from, to } => write!(f, "unknown {to:?} = {from:?}"),
      Inst::VarAssign { tgt, value } => write!(f, "%{tgt} = {value:?}"),
    }
  }
}
