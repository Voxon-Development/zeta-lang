use std::process::{Command, ExitStatus};

use crate::main_structs::CompilerError;

pub fn link<'a>(objects: &[&str], output: &str, link_libc: bool) -> Result<(), CompilerError<'a>> {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    let driver = "gcc";

    #[cfg(target_os = "windows")]
    let driver = "gcc"; // MinGW; otherwise use cl.exe or clang

    let mut cmd = Command::new(driver);

    cmd.args(objects);
    cmd.arg("-o").arg(output);

    if !link_libc {
        cmd.arg("-nostdlib");
    }

    let status: ExitStatus = cmd.status().expect("failed to execute linker");

    if !status.success() {
        return Err(CompilerError::LinkFailed);
    }

    Ok(())
}
