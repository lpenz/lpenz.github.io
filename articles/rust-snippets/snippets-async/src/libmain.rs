// Copyright information ...
use clap::Parser;
use color_eyre::Result;

use crate::cli::Cli;

#[tracing::instrument]
pub async fn main() -> Result<()> {
    color_eyre::install()?;
    tracing_subscriber::fmt()
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::ACTIVE)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    let args = Cli::parse();
    // Program goes here
    // Example trace:
    tracing::info!("args struct: {:?}", args);
    // Return Ok(()) on success
    Ok(())
}
