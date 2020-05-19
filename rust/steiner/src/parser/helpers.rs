use nom::error::{make_error, ErrorKind};
use nom::{Err, IResult};

// Takes the first element of the input vector and returns it
pub fn first<T: Clone + Copy>() -> impl Fn(Vec<T>) -> IResult<Vec<T>, T> {
    move |input: Vec<T>| {
        if let Some((first, rest)) = input.split_first() {
            Ok((rest.to_vec(), *first))
        } else {
            Err(Err::Error(make_error(input, ErrorKind::Eof)))
        }
    }
}

// Macro to only match a particular value
#[macro_export]
macro_rules! only {
    ($value:expr) => {
        verify(first(), |v| v == &$value)
    };
}

// Macro for matching a particular keyword
#[macro_export]
macro_rules! keyword {
    ($keyword:expr) => {
        only!(Token::Keyword($keyword))
    };
}

// Macro for matching a particular operator
#[macro_export]
macro_rules! operator {
    ($operator:expr) => {
        only!(Token::Operator($operator))
    };
}

#[macro_export]
macro_rules! punctuation {
    ($punctuation:expr) => {
        only!(Token::Punctuation($punctuation))
    };
}

// This matches any identifier
#[macro_export]
macro_rules! identifier {
    () => {
        map_opt(first(), |input| match input {
            Token::Identifier(value) => Some(value),
            _ => None,
        });
    };
}

// Shorthand for names of variables
pub type VariableName<'a> = &'a [u8];
