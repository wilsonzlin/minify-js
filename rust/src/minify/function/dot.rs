use super::inst::Inst;
use ahash::AHashMap;
use croaring::Bitmap;
use std::fmt::Write as FmtWrite;
use std::io::Write as IoWrite;
use std::process::Command;
use std::process::Stdio;

pub(crate) fn render_cfg(
  out: &str,
  bblock_order: &[u32],
  bblocks: &AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
) {
  let mut dot = String::new();

  writeln!(&mut dot, "digraph g {{").unwrap();
  writeln!(&mut dot, "graph [").unwrap();
  writeln!(&mut dot, r#"rankdir = "LR""#).unwrap();
  writeln!(&mut dot, "]").unwrap();
  for label in bblock_order {
    writeln!(&mut dot, r#""b{label}" ["#).unwrap();
    writeln!(&mut dot, r#"shape = "record""#).unwrap();
    write!(&mut dot, r#"label = "<f0>@{label}"#).unwrap();
    for (i, inst) in bblocks[label].iter().enumerate() {
      let inststr = format!("{inst:?}")
        .replace("\\", "\\\\")
        .replace("<", "\\<")
        .replace("\"", "\\\"")
        .replace("|", "\\|")
        .replace(">", "\\>")
        .replace("{", "\\{")
        .replace("}", "\\}");
      write!(&mut dot, r#"|<f{}>{inststr}"#, i + 1).unwrap();
    }
    writeln!(&mut dot, r#"""#).unwrap();
    writeln!(&mut dot, "];").unwrap();
  }
  for (label, children) in cfg_children {
    for c in children.iter() {
      writeln!(
        &mut dot,
        r#"b{label}:f{} -> b{c}:f0 [label="from @{label} to @{c}"];"#,
        bblocks[label].len()
      )
      .unwrap();
    }
  }
  writeln!(&mut dot, "}}").unwrap();

  let mut c = Command::new("dot")
    .arg("-Tpng")
    .arg("-o")
    .arg(out)
    .stdin(Stdio::piped())
    .stdout(Stdio::inherit())
    .stderr(Stdio::inherit())
    .spawn()
    .unwrap();
  let stdin = c.stdin.as_mut().unwrap();
  stdin.write_all(dot.as_bytes()).unwrap();
  stdin.flush().unwrap();
  c.wait().unwrap();
}
