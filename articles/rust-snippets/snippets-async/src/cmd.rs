use tokio::process::Command;

pub async fn run_commands() -> std::io::Result<()> {
    let output = Command::new("ls").args(["-l", "/"]).output().await?;
    println!("{:?}", output.status);
    println!("{:?}", output.stdout);
    Ok(())
}
