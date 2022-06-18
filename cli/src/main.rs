use minify_js::minify;
use std::io::{stdin, stdout, Read};

fn main() {
    let mut code = Vec::new();
    stdin().read_to_end(&mut code).expect("read stdin");
    minify(code.to_vec(), &mut stdout()).expect("minify");
}
