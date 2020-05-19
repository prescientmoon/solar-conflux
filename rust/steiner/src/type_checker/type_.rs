#[derive(Debug, Clone)]
pub enum Type<'a> {
    Constant(&'a [u8]),
    Lambda(Box<Type<'a>>, Box<Type<'a>>),
}

impl<'a> Type<'a> {
    pub fn create_lambda(from: Type<'a>, to: Type<'a>) -> Type<'a> {
        Type::Lambda(Box::new(from), Box::new(to))
    }
}
