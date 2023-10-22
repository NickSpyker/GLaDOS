use crate::code_syntax::Syntax;
use std::collections::BTreeMap;
use crate::haskell;
use regex::Regex;
use std::fs;

#[derive(Debug)]
pub struct SrcCode {
    buffer:   String,
    lines:    BTreeMap<usize, String>,
    codes:    BTreeMap<usize, String>
}

impl SrcCode {
    pub fn new(path: &str) -> Result<Self, String>
    {
        match fs::read_to_string(path) {
            Ok(buffer) => Ok(SrcCode {
                buffer,
                lines:    BTreeMap::new(),
                codes:    BTreeMap::new()
            }),
            Err(err) => Err(err.to_string())
        }
    }

    pub fn fetch_language_section_lines(&mut self) -> Result<(), String>
    {
        let re_ls: Regex = match Regex::new(
            r"(?m)^\s*parseWordToken\s*\(\s*([a-z:' ]+)\s*:\s*next\s*\)\s*=\s*Just\s*\(\s*([a-zA-Z() ]+)\s*,\s*next\s*\)\s*--\s*<([a-z]+)>\s*$"
        ) {
            Ok(regex) => regex,
            Err(err) => return Err(err.to_string())
        };

        for cap in re_ls.captures_iter(&self.buffer) {

            let key: usize;
            let value: String;

            if let Some(m) = cap.get(0) {
                key = self.buffer[..m.end()].chars().filter(|&c| c == '\n').count() + 1;
            } else { continue }
            if let Some(m) = cap.get(3) {
                value = m.as_str().to_string()
            } else { continue }

            self.lines.insert(key, value);
        }

        Ok(())
    }

    pub fn build_code(&mut self, dico: &Syntax) -> Result<(), String>
    {
        for (key, value) in &self.lines {
            let new_line: String = haskell::build_line_code(value, dico)?;
            self.codes.insert(*key, new_line);
        }

        Ok(())
    }

    pub fn build_function(&mut self)
    {
        let mut fun_lines: Vec<String> = Vec::new();
        for line in self.codes.values() {
            fun_lines.push(line.to_string());
        }
    }

    pub fn build_buffer(&self) -> Result<String, String>
    {
        let mut value: Vec<String> = Vec::new();
        for line_buffer in self.codes.values() {
            value.push(line_buffer.clone());
        }

        let first_line: usize = match self.codes.first_key_value() {
            Some((key, _)) => *key,
            None => return Err("unhandled error".to_string())
        };
        let last_line: usize = match self.codes.last_key_value() {
            Some((key, _)) => *key,
            None => return Err("unhandled error".to_string())
        };

        let mut first_part: String = String::new();
        let middle_part:    String = value.join("");
        let mut last_part:  String = String::new();

        let bv: Vec<&str> = self.buffer.split('\n').collect();

        for i in 0..bv.len() {
            if first_line <= i + 1 { break }
            first_part.push_str(&format!("{}\n", bv[i]));
        }
        for i in 0..bv.len() {
            if i < last_line { continue }
            last_part.push_str(&format!("{}\n", bv[i]));
        }
        last_part = last_part.trim().to_string();

        Ok(format!("{first_part}{middle_part}{last_part}\n"))
    }
}
