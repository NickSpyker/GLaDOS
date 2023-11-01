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
        "bool"   => Ok(dico.type_field .bool         .to_string()),
        "void"   => Ok(dico.type_field .void         .to_string()),
        "const"  => Ok(dico.biding     .constant     .to_string()),
        "char"   => Ok(dico.type_field .char         .to_string()),
        "int"    => Ok(dico.type_field .int          .to_string()),
        "in"     => Ok(dico.control    .in_field     .to_string()),
        "type"   => Ok(dico.biding     .new_type     .to_string()),
        "struct" => Ok(dico.biding     .structure    .to_string()),
        "enum"   => Ok(dico.biding     .enumerator   .to_string()),
        "use"    => Ok(dico.control    .import       .to_string()),
        "as"     => Ok(dico.control    .rename       .to_string()),
        _        => Err(format!("Invalid token {}", value))
    }
}

fn value_to_code(value: &str) -> Result<String, String>
{
    match value {
        "if"     => Ok("TokIf".to_string()),
        "or"     => Ok("TokOr".to_string()),
        "for"    => Ok("TokFor".to_string()),
        "not"    => Ok("TokNot".to_string()),
        "and"    => Ok("TokAnd".to_string()),
        "fun"    => Ok("TokFun".to_string()),
        "let"    => Ok("TokVar".to_string()),
        "else"   => Ok("TokElse".to_string()),
        "true"   => Ok("TokLit (LitBool True)".to_string()),
        "loop"   => Ok("TokLoop".to_string()),
        "while"  => Ok("TokWhile".to_string()),
        "break"  => Ok("TokBreak".to_string()),
        "false"  => Ok("TokLit (LitBool False)".to_string()),
        "return" => Ok("TokReturn".to_string()),
        "string" => Ok("TokTyString".to_string()),
        "float"  => Ok("TokTyFloat".to_string()),
        "bool"   => Ok("TokTyBool".to_string()),
        "void"   => Ok("TokTyVoid".to_string()),
        "const"  => Ok("TokConst".to_string()),
        "char"   => Ok("TokTyChar".to_string()),
        "int"    => Ok("TokTyInt".to_string()),
        "in"     => Ok("TokIn".to_string()),
        "type"   => Ok("TokDeclType".to_string()),
        "struct" => Ok("TokStruct".to_string()),
        "enum"   => Ok("TokEnum".to_string()),
        "use"    => Ok("TokImport".to_string()),
        "as"     => Ok("TokRName".to_string()),
        _        => Err(format!("Invalid token {}", value))
    }
}
