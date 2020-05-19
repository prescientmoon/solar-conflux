use crate::lexer::{KeywordKind, PunctuationKind, Token};
use nom::branch::alt;
use nom::combinator::{map, map_opt, opt, verify};
use nom::error::{make_error, ErrorKind};
use nom::sequence::delimited;
use nom::{Err, IResult};
use std::vec::Vec;

// Shorthand for names of variables
type VariableName<'a> = &'a [u8];

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Variable(VariableName<'a>),
    FloatLiteral(f64),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>),
    Let(VariableName<'a>, Box<Ast<'a>>, Box<Ast<'a>>),
    FunctionCall(Box<Ast<'a>>, Box<Ast<'a>>),
}

impl<'a> Ast<'a> {
    // Those 3 functions are just helpers to build expressions which contain Boxes
    pub fn new_if(condition: Ast<'a>, left: Ast<'a>, right: Ast<'a>) -> Ast<'a> {
        Ast::If(Box::new(condition), Box::new(left), Box::new(right))
    }

    pub fn new_let(name: VariableName<'a>, value: Ast<'a>, body: Ast<'a>) -> Ast<'a> {
        Ast::Let(name, Box::new(value), Box::new(body))
    }

    pub fn new_call(function: Ast<'a>, argument: Ast<'a>) -> Ast<'a> {
        Ast::FunctionCall(Box::new(function), Box::new(argument))
    }
}

// Takes the first element of the input vector and returns it
fn first<T: Clone + Copy>() -> impl Fn(Vec<T>) -> IResult<Vec<T>, T> {
    move |input: Vec<T>| {
        if let Some((first, rest)) = input.split_first() {
            Ok((rest.to_vec(), *first))
        } else {
            Err(Err::Error(make_error(input, ErrorKind::Eof)))
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

// Macro for matching a particular operator
macro_rules! operator {
    ($operator:expr) => {
        only!(Token::Operator($operator))
    };
}

macro_rules! punctuation {
    ($punctuation:expr) => {
        only!(Token::Punctuation($punctuation))
    };
}

// This matches any identifier
macro_rules! identifier {
    () => {
        map_opt(first(), |input| match input {
            Token::Identifier(value) => Some(value),
            _ => None,
        });
    };
}

// This parses an if statement
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

// This parses a let expression
fn parse_let(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let (remaining, (_, name, _, value, _, body)) = nom::sequence::tuple((
        keyword!(KeywordKind::Let),
        identifier!(),
        operator!(b"="),
        parse_expression,
        keyword!(KeywordKind::In),
        parse_expression,
    ))(input)?;

    let ast = Ast::new_let(name, value, body);

    Ok((remaining, ast))
}

fn maybe_call<'a>(expression: Ast<'a>, input: Vec<Token<'a>>) -> IResult<Vec<Token<'a>>, Ast<'a>> {
    map(opt(parse_expression), |result| {
        let expression_clone = expression.clone();
        match result {
            Some(argument) => Ast::new_call(expression_clone, argument),
            None => expression_clone,
        }
    })(input)
}

pub fn parse_expression_with_calls<'a>(input: Vec<Token<'a>>) -> IResult<Vec<Token>, Ast> {
    let (rest, expression) = parse_expression(input)?;
    maybe_call(expression, rest)
}

pub fn parse_expression(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let parse_float_literal = map_opt(first(), |input| match input {
        Token::FloatLit(value) => Some(Ast::FloatLiteral(value)),
        _ => None,
    });

    let prase_wrapped = delimited(
        punctuation!(PunctuationKind::OpenParenthesis),
        parse_expression_with_calls,
        punctuation!(PunctuationKind::CloseParenthesis),
    );

    let parse_identifier = map(identifier!(), Ast::Variable);

    let parse = alt((
        parse_if,
        parse_let,
        parse_float_literal,
        parse_identifier,
        prase_wrapped,
    ));

    parse(input)
}
