use std::path::PathBuf;
use std::process::Command;

use super::{Error, ResultExt};

fn find_runtime_library() -> Result<String, Error> {
    let this_binary = PathBuf::from(::std::env::args().next().unwrap());
    let this_binary_dir = this_binary.parent().unwrap_or(".".as_ref());
    let candidate = this_binary_dir.join("libminivm_rt.a");
    if !candidate.exists() {
        bail!("runtime library not found")
    }
    Ok(candidate.to_string_lossy().into())
}

fn find_linker() -> Result<String, Error> {
    Ok("cc".to_string())
}

#[cfg(target_os = "macos")]
static DEFAULT_LD_FLAGS: [&'static str; 4] = ["-lSystem", "-lresolv", "-lc", "-lm"];

#[cfg(target_os = "linux")]
static DEFAULT_LD_FLAGS: [&'static str; 9] = ["-ldl",
                                              "-lrt",
                                              "-pthread",
                                              "-lgcc_s",
                                              "-lc",
                                              "-lm",
                                              "-lrt",
                                              "-lpthread",
                                              "-lutil"];

pub fn link(executable: &str, obj: &str) -> Result<(), Error> {
    let linker = find_linker()?;
    let runtime = find_runtime_library()?;

    let mut args = vec!["-o", executable, obj, &runtime];
    args.extend(DEFAULT_LD_FLAGS.iter());

    let status = Command::new(linker)
        .args(&args)
        .status()
        .chain_err(|| "failed to execute linker")?;

    if !status.success() {
        bail!("linker failed with exit status {}", status.code().unwrap());
    }

    Ok(())
}
