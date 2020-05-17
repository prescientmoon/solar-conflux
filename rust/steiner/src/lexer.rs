use nom::branch::alt;
use nom::bytes::complete::is_a;
use nom::character::complete::one_of;
use nom::character::complete::{alphanumeric1, multispace0};
use nom::combinator::{all_consuming, map};
use nom::multi::many0;
use nom::number::complete::double;
use nom::sequence::terminated;
use nom::IResult;
use std::vec::Vec;

#[derive(Debug)]
pub enum PunctuationKind {
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug)]
pub enum TokenKind<'a> {
    FloatLit(f64),
    Punctuation(PunctuationKind),
    Operator(&'a [u8]),
    Identifier(&'a [u8]),
}

// Convert a char to it's corresponding punctuation
fn char_to_punctuation<'a>(input: char) -> TokenKind<'a> {
    TokenKind::Punctuation(match input {
        '(' => PunctuationKind::OpenParenthesis,
        ')' => PunctuationKind::CloseParenthesis,
        _ => panic!("{} is not a valid punctuation character!", input),
    })
}

const OPERATOR_CHARS: &[u8] = b"<=>+-/*!$%^&|";

pub fn lex(input: &[u8]) -> IResult<&[u8], Vec<TokenKind>> {
    let parse_float_literal = map(double, TokenKind::FloatLit);
    let parse_punctuation = map(one_of("()"), char_to_punctuation);
    let parse_operator = map(is_a(OPERATOR_CHARS), TokenKind::Operator);
    let parse_identifier = map(alphanumeric1, TokenKind::Identifier);

    let parse = all_consuming(many0(terminated(
        alt((
            parse_float_literal,
            parse_punctuation,
            parse_operator,
            parse_identifier,
        )),
        multispace0,
    )));

    parse(input)
}
