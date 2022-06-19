use minify_js::minify;
use std::{
    env,
    fs::File,
    io::{Read, BufWriter}, time::Instant,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut code = Vec::new();
    File::open(&args[1]).expect("open file").read_to_end(&mut code).expect("read file");

    let iterations = u64::from_str_radix(&args[2], 10).expect("parse iterations argument");
    let mut output_len = 0;
    let mut output = BufWriter::new(Vec::new());
    let started = Instant::now();
    for _ in 0..iterations {
      output.get_mut().clear();
      minify(code.to_vec(), &mut output).expect("minify");
      output_len = output.get_ref().len();
    }
    let elapsed_ns = started.elapsed().as_nanos();

    println!("{} {}", output_len, elapsed_ns);
}
