use crate::parser::helpers::VariableName;

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Constant(VariableName<'a>),
    Lambda(Box<Type<'a>>, Box<Type<'a>>),
    Variable(VariableName<'a>),
}

impl<'a> Type<'a> {
    pub fn create_lambda(from: Type<'a>, to: Type<'a>) -> Type<'a> {
        Type::Lambda(Box::new(from), Box::new(to))
    }
}
