use crate::code_syntax::Syntax;
use std::collections::HashMap;
use walkdir::WalkDir;

#[derive(Debug)]
pub struct Samples {
    collection: HashMap<String, Syntax>
}

impl Samples {
    pub fn parse(path: &str) -> Self
    {
        let mut collection: HashMap<String, Syntax> = HashMap::new();
        let dir: WalkDir = WalkDir::new(path);

        for entry in dir.into_iter().filter_map(|entry| entry.ok()) {
            if !entry.file_type().is_file() { continue }
            if let Some(extension) = entry.path().extension() {
                if extension != "json" { continue }
                let file_path: &str = match entry.path().to_str() {
                    Some(path) => path,
                    None => continue
                };
                let file_name: String = match entry.path().file_stem() {
                    Some(name) => match name.to_str() {
                        Some(name) => name.to_string(),
                        None => continue
                    }
                    None => continue
                };
                match Syntax::parse(&file_path) {
                    Ok(syntax) => {
                        collection.insert(file_name, syntax);
                    },
                    Err(_) => continue
                }
            }
        }

        Samples { collection }
    }

    pub fn get_language(&self, language: &str) -> Result<Syntax, String>
    {
        match self.collection.get(language) {
            Some(value) => Ok(value.clone()),
            None => Err(format!("missing {language}.json file"))
        }
    }
}
