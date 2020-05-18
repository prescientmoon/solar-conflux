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

macro_rules! only {
    ($value:expr) => {
        verify(first(), |v| v == &$value)
    };
}

fn parse_if(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let (remaining, (_, condition, _, left, _, right)) = nom::sequence::tuple((
        only!(Token::Keyword(KeywordKind::If)),
        parse_expression,
        only!(Token::Keyword(KeywordKind::Then)),
        parse_expression,
        only!(Token::Keyword(KeywordKind::Else)),
        parse_expression,
    ))(input)?;

    Ok((remaining, Ast::new_if(condition, left, right)))
}

pub fn parse_expression(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let parse_float_literal = map_opt(first(), |input| match input {
        Token::FloatLit(value) => Some(Ast::FloatLiteral(value)),
        _ => None,
    });
    let parse = alt((parse_if, parse_float_literal));
    parse(input)
}
