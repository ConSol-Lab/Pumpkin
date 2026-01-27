use std::error::Error;

fn main() {
    if let Err(e) = run() {
        fail(&e.to_string());
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-env-changed=NO_CHECKERS");

    Ok(())
}

fn fail(s: &str) -> ! {
    eprintln!("\n\nerror occurred: {s}\n\n");
    std::process::exit(1);
}
