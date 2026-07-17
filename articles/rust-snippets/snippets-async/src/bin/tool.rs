// Copyright information ...

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    snippets::main().await?;
    Ok(())
}
