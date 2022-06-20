use minify_js::minify;
use std::fs::File;
use std::io::{stdin, stdout, BufWriter, Read, Write};

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
}

fn main() {
    let args = Cli::from_args();
    let mut input = Vec::new();
    let mut input_file: Box<dyn Read> = match args.input {
        Some(p) => Box::new(File::open(p).expect("open input file")),
        None => Box::new(stdin()),
    };
    input_file.read_to_end(&mut input).expect("read input");
    let out_file: Box<dyn Write> = match args.output {
        Some(p) => Box::new(File::create(p).expect("open output file")),
        None => Box::new(stdout()),
    };
    let mut output = BufWriter::new(out_file);
    minify(input.to_vec(), &mut output).expect("minify");
}
