use crate::compile::Type;

#[derive(Debug)]
pub struct Stdlib {
  pub module_name: String,
  pub name:        String,
  pub return_type: Type,
}
pub fn get_stdlib(name: &String, args: &Vec<Type>) -> Option<Stdlib> {
  let return_type = match (name.as_str(), args.as_slice()) {
    ("print", v) | ("println", v) => {
      if v.len() > 0 && v[0] == Type::StrLiteral {
        Type::Void
      } else {
        return None;
      }
    }
    ("read", []) => Type::I64,
    ("random", [Type::I64, Type::I64]) => Type::I64,
    _ => return None,
  };
  Some(Stdlib {
    module_name: "std".to_string(),
    name: name.clone(),
    return_type,
  })
}
