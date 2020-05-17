use nom::character::complete::space0;
use nom::number::complete::double;
use nom::{alt, is_a, many0, map, named, one_of, terminated};
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

named!(parse_float_literal<&[u8],TokenKind>,map!(double, |f| TokenKind::FloatLit(f)));
named!(parse_punctuation<&[u8], TokenKind>, map!(one_of!("()"), char_to_punctuation));

named!(parse_operator<&[u8],TokenKind>, map!(is_a!(OPERATOR_CHARS),|operator| TokenKind::Operator(operator)));

named!(
    pub lex<&[u8], Vec<TokenKind>>,
    many0!(terminated!(alt!(parse_float_literal | parse_punctuation | parse_operator), space0))
);
