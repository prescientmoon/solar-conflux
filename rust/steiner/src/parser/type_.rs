use crate::lexer::{PunctuationKind, Token};
use crate::parser::helpers::first;
use crate::type_checker::type_::Type;
use nom::branch::alt;
use nom::combinator::{map, map_opt, verify};
use nom::sequence::{delimited, tuple};
use nom::IResult;
use std::vec::Vec;

use crate::{identifier, operator};

fn parse_non_lambda_type(input: Vec<Token>) -> IResult<Vec<Token>, Type> {
    let prase_wrapped = delimited(
        punctuation!(PunctuationKind::OpenParenthesis),
        parse_type,
        punctuation!(PunctuationKind::CloseParenthesis),
    );

    let parse_identifier = map(identifier!(), Type::Constant);

    let parse = alt((prase_wrapped, parse_identifier));

    parse(input)
}

fn parse_lambda(input: Vec<Token>) -> IResult<Vec<Token>, Type> {
    map(
        tuple((parse_non_lambda_type, operator!(b"->"), parse_type)),
        |(from, _, to)| Type::create_lambda(from, to),
    )(input)
}

pub fn parse_type(input: Vec<Token>) -> IResult<Vec<Token>, Type> {
    let parse = alt((parse_lambda, parse_non_lambda_type));

    parse(input)
}
