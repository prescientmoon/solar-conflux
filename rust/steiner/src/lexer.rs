use nom::branch::alt;
use nom::bytes::complete::{is_a, tag};
use nom::character::complete::{alphanumeric1, multispace0, space1};
use nom::combinator::{all_consuming, map, map_opt, value};
use nom::multi::many0;
use nom::number::complete::double;
use nom::sequence::terminated;
use nom::IResult;
use std::vec::Vec;

// This is a simple macro for making parsers which replace a tag with a custom value
macro_rules! tagged {
    ($tag:expr,$value:expr) => {
        value($value, tag($tag))
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PunctuationKind {
    OpenParenthesis,
    CloseParenthesis,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordKind {
    If,
    Then,
    Else,
}

macro_rules! keyword {
    ($tag: expr, $keyword: expr) => {
        value($keyword, terminated(tag($tag), space1))
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    FloatLit(f64),
    Punctuation(PunctuationKind),
    Keyword(KeywordKind),
    Operator(&'a [u8]),
    Identifier(&'a [u8]),
}

const OPERATOR_CHARS: &[u8] = b"<=>+-/*!$%^&|";
const KEYWORDS: [&[u8]; 3] = [b"if", b"then", b"else"];

pub fn lex(input: &[u8]) -> IResult<&[u8], Vec<Token>> {
    let parse_float_literal = map(double, Token::FloatLit);
    let parse_punctuation = map(
        alt((
            tagged!(b"(", PunctuationKind::OpenParenthesis),
            tagged!(b")", PunctuationKind::CloseParenthesis),
        )),
        Token::Punctuation,
    );

    let parse_keyword = map(
        alt((
            keyword!(KEYWORDS[0], KeywordKind::If),
            keyword!(KEYWORDS[1], KeywordKind::Then),
            keyword!(KEYWORDS[2], KeywordKind::Else),
        )),
        Token::Keyword,
    );

    let parse_operator = map(is_a(OPERATOR_CHARS), Token::Operator);
    let parse_identifier = map_opt(alphanumeric1, |id| {
        if KEYWORDS.contains(&id) {
            None
        } else {
            Some(Token::Identifier(id))
        }
    });

    let parse = all_consuming(many0(terminated(
        alt((
            parse_float_literal,
            parse_punctuation,
            parse_keyword,
            parse_operator,
            parse_identifier,
        )),
        multispace0,
    )));

    parse(input)
}
