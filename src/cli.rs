use clap::Parser;

#[derive(Debug, Parser)]
#[command(
    name = "amaii",
    bin_name = "amaii",
    about = "The Amai interpreter/VM",
)]
pub struct Cli {
    #[arg(required = true, num_args = 1..)]
    pub input: String,

    #[arg(short, long)]
    pub debug: bool,
}