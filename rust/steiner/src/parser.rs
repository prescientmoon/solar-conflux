use crate::lexer::{KeywordKind, Token};
use nom::branch::alt;
use nom::combinator::{map_opt, verify};
use nom::error::{make_error, ErrorKind};
use nom::{Err, IResult};
use std::vec::Vec;

#[derive(Debug)]
pub enum Ast<'a> {
    Variable(&'a [u8]),
    FloatLiteral(f64),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>),
}

impl<'a> Ast<'a> {
    pub fn new_if(condition: Ast<'a>, left: Ast<'a>, right: Ast<'a>) -> Ast<'a> {
        Ast::If(Box::new(condition), Box::new(left), Box::new(right))
    }
}

fn first<T: Clone + Copy>() -> impl Fn(Vec<T>) -> IResult<Vec<T>, T> {
    move |input: Vec<T>| {
        if let Some((first, rest)) = input.split_first() {
            Ok((rest.to_vec(), *first))
        } else {
            Err(Err::Error(make_error(input, ErrorKind::Tag)))
        }
    }
}

// Macro to only match a particular value
macro_rules! only {
    ($value:expr) => {
        verify(first(), |v| v == &$value)
    };
}

// Macro for matching a particular keyword
macro_rules! keyword {
    ($keyword:expr) => {
        only!(Token::Keyword($keyword))
    };
}

fn parse_if(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let (remaining, (_, condition, _, left, _, right)) = nom::sequence::tuple((
        keyword!(KeywordKind::If),
        parse_expression,
        keyword!(KeywordKind::Then),
        parse_expression,
        keyword!(KeywordKind::Else),
        parse_expression,
    ))(input)?;

    let ast = Ast::new_if(condition, left, right);

    Ok((remaining, ast))
}

pub fn parse_expression(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let parse_float_literal = map_opt(first(), |input| match input {
        Token::FloatLit(value) => Some(Ast::FloatLiteral(value)),
        _ => None,
    });

    let parse_identifier = map_opt(first(), |input| match input {
        Token::Identifier(value) => Some(Ast::Variable(value)),
        _ => None,
    });

    let parse = alt((parse_if, parse_float_literal, parse_identifier));

    parse(input)
}
