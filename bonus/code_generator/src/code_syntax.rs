use serde::{Serialize, Deserialize};
use std::fs;

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Syntax {
    #[serde(rename = "type")]
    pub type_field: Type,
    pub logic:      Logic,
    pub control:    Control,
    pub biding:     Biding,
    pub value:      Value
}

impl Syntax {
    pub fn parse(path: &str) -> Result<Self, String>
    {
        let buffer: String = match fs::read_to_string(path) {
            Ok(buffer) => buffer,
            Err(err) => return Err(err.to_string())
        };

        match serde_json::from_str::<Syntax>(&buffer) {
            Ok(result) => Ok(result),
            Err(err) => Err(err.to_string())
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Type {
    pub string: String,
    pub float:  String,
    pub bool:   String,
    pub char:   String,
    pub int:    String
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Logic {
    pub and: String,
    pub or:  String,
    pub not: String
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Control {
    #[serde(rename = "if")]
    pub if_field:       String,
    #[serde(rename = "else")]
    pub else_field:     String,
    #[serde(rename = "for")]
    pub for_field:      String,
    #[serde(rename = "in")]
    pub in_field:       String,
    #[serde(rename = "loop")]
    pub loop_field:     String,
    #[serde(rename = "while")]
    pub while_field:    String,
    #[serde(rename = "break")]
    pub break_field:    String,
    #[serde(rename = "continue")]
    pub continue_field: String,
    #[serde(rename = "return")]
    pub return_field:   String
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Biding {
    pub variable: String,
    pub constant: String,
    pub function: String
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Value {
    #[serde(rename = "true")]
    pub true_field:  String,
    #[serde(rename = "false")]
    pub false_field: String
}
