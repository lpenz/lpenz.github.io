// Copyright information ...
use clap::Parser;
use color_eyre::Result;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    // Add command-line arguments to this struct, with documentation
}

#[tracing::instrument]
pub fn main() -> Result<()> {
    let args = Cli::parse();
    // Program goes here
    // Example trace:
    tracing::info!("args struct: {:?}", args);
    // Return Ok(()) on success
    Ok(())
}
