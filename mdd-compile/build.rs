use cmake::Config;

fn main() {
    let mut dst = Config::new("minicpp").no_build_target(true).build();
    dst.push("build");
    println!("cargo:rustc-link-search=native={}", dst.display());
    println!("cargo:rustc-link-lib=static=copl");
}
