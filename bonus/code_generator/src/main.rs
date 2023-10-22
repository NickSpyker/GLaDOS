mod code_syntax;
mod src_code;
mod samples;
mod env;

use code_syntax::Syntax;
use src_code::SrcCode;
use samples::Samples;

fn main() -> Result<(), String>
{
    let args: Vec<String> = std::env::args().collect();

    if 2 < args.len() {
        eprintln!("Please, only one language is wanted for GLaDOS.");
        return Err(format!("there are too many inputs, got {}, expected 1", args.len() - 1));
    }

    let language: String = match args.get(1) {
        Some(language) => language.to_owned(),
        None => env::get_default_language()
    };

    let syntax: Syntax = Samples::parse(&env::get_samples_directory()).get_language(&language)?;
    let code: SrcCode = SrcCode::new(&env::get_target_file())?;

    println!("{:#?}", syntax);

    Ok(())
}
