#[macro_use]
pub mod helpers;
mod type_;

use crate::lexer::{KeywordKind, PunctuationKind, Token};
use crate::type_checker::type_::Type;
use helpers::{first, VariableName};
use nom::branch::alt;
use nom::combinator::{map, map_opt, verify};
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Variable(VariableName<'a>),
    FloatLiteral(f64),
    If(Box<Ast<'a>>, Box<Ast<'a>>, Box<Ast<'a>>),
    Let(VariableName<'a>, Box<Ast<'a>>, Box<Ast<'a>>),
    FunctionCall(Box<Ast<'a>>, Box<Ast<'a>>),
    Lambda(VariableName<'a>, Box<Ast<'a>>),
    Annotation(Box<Ast<'a>>, Type),
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

    pub fn new_lambda(name: VariableName<'a>, body: Ast<'a>) -> Ast<'a> {
        Ast::Lambda(name, Box::new(body))
    }

    // construct a chain of function calls
    pub fn chain_call(self: Ast<'a>, arguments: Vec<Ast<'a>>) -> Ast<'a> {
        let mut result = self;

        for argument in arguments {
            result = Ast::new_call(result, argument)
        }

        result
    }

    // construct a chain of lambda declarations
    pub fn lambda_chain(self: Ast<'a>, parameters: Vec<VariableName<'a>>) -> Ast<'a> {
        let mut result = self;

        for parameter in parameters {
            result = Ast::new_lambda(parameter, result)
        }

        result
    }

    // annotate an expression with a type
    pub fn annotate(self: Ast<'a>, annotation: Type) -> Ast<'a> {
        Ast::Annotation(Box::new(self), annotation)
    }
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

// Parse the argument list for a function
fn parse_argument_list(input: Vec<Token>) -> IResult<Vec<Token>, Vec<VariableName<'_>>> {
    many0(identifier!())(input)
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

// Prase a lambda
fn parse_lambda(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let (remaining, (_, parameters, _, body)) = nom::sequence::tuple((
        punctuation!(PunctuationKind::Backslash),
        parse_argument_list,
        operator!(b"->"),
        parse_expression,
    ))(input)?;

    let ast = body.lambda_chain(parameters);

    Ok((remaining, ast))
}

pub fn parse_expression<'a>(input: Vec<Token<'a>>) -> IResult<Vec<Token>, Ast> {
    let (rest, expression) = parse_atom(input)?;
    let (rest, arguments) = many0(parse_atom)(rest)?;

    let ast = expression.chain_call(arguments);

    Ok((rest, ast))
}

pub fn parse_atom(input: Vec<Token>) -> IResult<Vec<Token>, Ast> {
    let prase_wrapped = delimited(
        punctuation!(PunctuationKind::OpenParenthesis),
        parse_expression,
        punctuation!(PunctuationKind::CloseParenthesis),
    );

    let parse_float_literal = map_opt(first(), |input| match input {
        Token::FloatLit(value) => Some(Ast::FloatLiteral(value)),
        _ => None,
    });

    let parse_identifier = map(identifier!(), Ast::Variable);

    let parse = alt((
        parse_if,
        parse_let,
        parse_lambda,
        parse_float_literal,
        parse_identifier,
        prase_wrapped,
    ));

    let (rest, expression) = parse(input)?;

    match preceded(
        punctuation!(PunctuationKind::DoubleColon),
        type_::parse_type,
    )(rest.clone())
    {
        Ok((rest, annotation)) => Ok((rest, expression.annotate(annotation))),
        Err(_) => Ok((rest, expression)),
    }
}
