use crate::code_syntax::Syntax;

pub fn build_line_code(value: &str, dico: &Syntax) -> Result<String, String>
{
    let tr_value: String = translate(value, dico)?;
    let mut tr: Vec<String> = Vec::new();
    for c in tr_value.chars() {
        let c_str: String = c.to_string();
        tr.push(format!("'{c_str}' : "));
    }

    let values: String = tr.join("");
    let out: String = value_to_code(value)?;

    Ok(format!("parseWordToken ({values}next) = Just ({out}, next) -- <{value}>\n"))
}

fn translate(value: &str, dico: &Syntax) -> Result<String, String>
{
    match value {
        "if"     => Ok(dico.control    .if_field     .to_string()),
        "or"     => Ok(dico.logic      .or           .to_string()),
        "for"    => Ok(dico.control    .for_field    .to_string()),
        "not"    => Ok(dico.logic      .not          .to_string()),
        "and"    => Ok(dico.logic      .and          .to_string()),
        "fun"    => Ok(dico.biding     .function     .to_string()),
        "let"    => Ok(dico.biding     .variable     .to_string()),
        "else"   => Ok(dico.control    .else_field   .to_string()),
        "true"   => Ok(dico.value      .true_field   .to_string()),
        "loop"   => Ok(dico.control    .loop_field   .to_string()),
        "while"  => Ok(dico.control    .while_field  .to_string()),
        "break"  => Ok(dico.control    .break_field  .to_string()),
        "false"  => Ok(dico.value      .false_field  .to_string()),
        "return" => Ok(dico.control    .return_field .to_string()),
        "string" => Ok(dico.type_field .string       .to_string()),
        "float"  => Ok(dico.type_field .float        .to_string()),
        "const"  => Ok(dico.biding     .constant     .to_string()),
        "char"   => Ok(dico.type_field .char         .to_string()),
        "int"    => Ok(dico.type_field .int          .to_string()),
        "in"     => Ok(dico.control    .in_field     .to_string()),
        _        => Err(format!("Invalid token {}", value))
    }
}

fn value_to_code(value: &str) -> Result<String, String>
{
    match value {
        "if"     => Ok("If".to_string()),
        "or"     => Ok("Or".to_string()),
        "for"    => Ok("For".to_string()),
        "not"    => Ok("Not".to_string()),
        "and"    => Ok("And".to_string()),
        "fun"    => Ok("Fun".to_string()),
        "let"    => Ok("Var".to_string()),
        "else"   => Ok("Else".to_string()),
        "true"   => Ok("Lit (LitBool True)".to_string()),
        "loop"   => Ok("Loop".to_string()),
        "while"  => Ok("While".to_string()),
        "break"  => Ok("Break".to_string()),
        "false"  => Ok("Lit (LitBool False)".to_string()),
        "return" => Ok("Return".to_string()),
        "string" => Ok("TyString".to_string()),
        "float"  => Ok("TyFloat".to_string()),
        "const"  => Ok("Const".to_string()),
        "char"   => Ok("TyChar".to_string()),
        "int"    => Ok("TyInt".to_string()),
        "in"     => Ok("In".to_string()),
        _        => Err(format!("Invalid token {}", value))
    }
}
