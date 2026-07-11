use crate::CompilerError;
use std::process::{Command, ExitStatus};

pub fn link<'a>(objects: &[&str], output: &str, link_libc: bool) -> Result<(), CompilerError<'a>> {
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    let driver = "cc";

    #[cfg(target_os = "windows")]
    let driver = "cl.exe";

    let mut cmd = Command::new(driver);

    // driver handles all crt, libc, dynamic loader details
    cmd.arg("-o").arg(output);
    cmd.args(objects);

    if !link_libc {
        // static or freestanding mode
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        cmd.arg("-nostdlib");

        #[cfg(target_os = "windows")]
        cmd.arg("/NODEFAULTLIB");
    }

    let status: ExitStatus = cmd.status().expect("failed to execute linker");
    if !status.success() {
        return Err(CompilerError::LinkFailed);
    }

    Ok(())
}
