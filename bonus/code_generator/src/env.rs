use dotenv_codegen::dotenv;

pub fn get_default_language() -> String
{
    dotenv!("DEFAULT_LANGUAGE").to_string()
}

pub fn get_samples_directory() -> String
{
    dotenv!("SAMPLES_DIR").to_string()
}

pub fn get_target_file() -> String
{
    dotenv!("TARGET_FILE").to_string()
}
