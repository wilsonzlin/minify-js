macro_rules! visit_inst_tgts {
  ($inst:expr, | $tgt:ident | $process:expr) => {
    match $inst {
      Inst::Bin { tgt, .. } => {
        let $tgt = tgt;
        $process;
      }
      Inst::Un { tgt, .. } => {
        let $tgt = tgt;
        $process;
      }
      Inst::VarAssign { tgt, .. } => {
        let $tgt = tgt;
        $process;
      }
      Inst::PropAssign { .. } => {}
      Inst::Goto { .. } => {}
      Inst::CondGoto { .. } => {}
      Inst::NotCondGoto { .. } => {}
      Inst::Call { tgt, .. } => {
        if let Some(tgt) = tgt {
          let $tgt = tgt;
          $process;
        };
      }
      Inst::ForeignLoad { to, .. } => {
        let $tgt = to;
        $process;
      }
      Inst::ForeignStore { .. } => {}
      Inst::UnknownLoad { to, .. } => {
        let $tgt = to;
        $process;
      }
      Inst::UnknownStore { .. } => {}
      Inst::Phi { tgt, .. } => {
        let $tgt = tgt;
        $process;
      }
      Inst::Label { .. } => {}
    }
  };
}

pub(crate) use visit_inst_tgts;

macro_rules! visit_inst_args {
  ($inst:expr, | $arg:ident | $proc:expr) => {
    match $inst {
      Inst::Bin { left, right, .. } => {
        let $arg = left;
        $proc;
        let $arg = right;
        $proc;
      }
      Inst::Un { arg, .. } => {
        let $arg = arg;
        $proc;
      }
      Inst::VarAssign { value, .. } => {
        let $arg = value;
        $proc;
      }
      Inst::PropAssign { obj, prop, value } => {
        let $arg = obj;
        $proc;
        let $arg = prop;
        $proc;
        let $arg = value;
        $proc;
      }
      Inst::Goto { .. } => {}
      Inst::CondGoto { cond, .. } => {
        let $arg = cond;
        $proc;
      }
      Inst::NotCondGoto { cond, .. } => {
        let $arg = cond;
        $proc;
      }
      Inst::Call {
        func, this, args, ..
      } => {
        let $arg = func;
        $proc;
        let $arg = this;
        $proc;
        for a in args {
          match a {
            CallArg::Spread(a) | CallArg::Arg(a) => {
              let $arg = a;
              $proc;
            }
          }
        }
      }
      Inst::ForeignLoad { .. } => {}
      Inst::ForeignStore { from, .. } => {
        let $arg = from;
        $proc;
      }
      Inst::UnknownLoad { .. } => {}
      Inst::UnknownStore { from, .. } => {
        let $arg = from;
        $proc;
      }
      Inst::Phi { from_blocks, .. } => {
        for (_, a) in from_blocks {
          let $arg = a;
          $proc;
        }
      }
      Inst::Label { .. } => {}
    }
  };
}

pub(crate) use visit_inst_args;
