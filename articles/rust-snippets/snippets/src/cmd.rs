use std::io::{BufRead, BufReader, Error, ErrorKind};
use std::process::{Command, Stdio};

pub fn run_commands() -> std::io::Result<()> {
    let mut child = Command::new("ls")
        .args(["-l", "/"])
        .stdout(Stdio::piped())
        .spawn()?;
    let stdout = child
        .stdout
        .as_mut()
        .ok_or(Error::from(ErrorKind::BrokenPipe))?;
    for line in BufReader::new(stdout).lines() {
        println!("{:?}", line);
    }
    let result = child.wait()?;
    println!("{:?}", result);
    Ok(())
}
