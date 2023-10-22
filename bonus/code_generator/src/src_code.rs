use std::fs;

#[derive(Debug)]
pub struct SrcCode {
    buffer: String
}

impl SrcCode {
    pub fn new(path: &str) -> Result<Self, String>
    {
        match fs::read_to_string(path) {
            Ok(buffer) => Ok(SrcCode { buffer }),
            Err(err) => Err(err.to_string())
        }
    }
}
