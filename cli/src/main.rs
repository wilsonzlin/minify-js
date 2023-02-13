use minify_js::minify;
use minify_js::Session;
use minify_js::TopLevelMode;
use std::fs::File;
use std::io::stdin;
use std::io::stdout;
use std::io::Read;
use std::io::Write;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "minify-js", about = "Extremely fast JS minifier")]
// WARNING: Keep descriptions in sync with Cfg.
struct Cli {
  /// File to minify; omit for stdin.
  #[structopt(parse(from_os_str))]
  input: Option<std::path::PathBuf>,

  /// Output destination; omit for stdout.
  #[structopt(short, long, parse(from_os_str))]
  output: Option<std::path::PathBuf>,

  /// Whether file is a module or global script.
  #[structopt(short, long)]
  mode: TopLevelMode,
}

fn main() {
  let args = Cli::from_args();
  let mut input = Vec::new();
  let mut input_file: Box<dyn Read> = match args.input {
    Some(p) => Box::new(File::open(p).expect("open input file")),
    None => Box::new(stdin()),
  };
  input_file.read_to_end(&mut input).expect("read input");
  let mut output = Vec::new();
  let session = Session::new();
  minify(&session, args.mode, &input, &mut output).expect("minify");
  match args.output {
    Some(p) => File::create(p)
      .expect("open output file")
      .write_all(&output),
    None => stdout().write_all(&output),
  }
  .expect("write output");
}
