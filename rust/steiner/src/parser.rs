use crate::type_checker::type_::{SchemeVar, Type, VarName};
use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum Ast {
    Variable(String),
    FloatLiteral(f64),
    StringLiteral(String),
    If(Box<Ast>, Box<Ast>, Box<Ast>),
    Let(String, Box<Ast>, Box<Ast>),
    FunctionCall(Box<Ast>, Box<Ast>),
    Lambda(String, Box<Ast>),
    Annotation(Box<Ast>, Type),
}

impl Ast {
    // Constructors
    pub fn new_if(condition: Ast, left: Ast, right: Ast) -> Ast {
        Ast::If(Box::new(condition), Box::new(left), Box::new(right))
    }

    pub fn new_let(name: String, value: Ast, body: Ast) -> Ast {
        Ast::Let(name, Box::new(value), Box::new(body))
    }

    pub fn new_call(function: Ast, argument: Ast) -> Ast {
        Ast::FunctionCall(Box::new(function), Box::new(argument))
    }

    pub fn new_lambda(name: String, body: Ast) -> Ast {
        Ast::Lambda(name, Box::new(body))
    }

    pub fn call_chain(self: Ast, arguments: Vec<Ast>) -> Ast {
        let mut result = self;

        for argument in arguments {
            result = Ast::new_call(result, argument)
        }

        result
    }

    pub fn lambda_chain(self: Ast, parameters: Vec<String>) -> Ast {
        let mut result = self;

        for parameter in parameters.iter().rev() {
            result = Ast::new_lambda(parameter.clone(), result)
        }

        result
    }

    // annotate an expression with a type
    pub fn annotate(self: Ast, annotation: Type) -> Ast {
        Ast::Annotation(Box::new(self), annotation)
    }

    // annotate with multiple types
    pub fn annotate_many(self: Ast, annotations: Vec<Type>) -> Ast {
        let mut result = self;

        for annotation in annotations {
            result = result.annotate(annotation)
        }

        result
    }
}

// If this is true the string cannot be used as a variable name and stuff
fn is_reserved(input: &str) -> bool {
    common_macros::hash_set!["if", "then", "else", "let", "in", "forall"].contains(input)
}

peg::parser! {
    grammar parse() for str {
        rule whitespace() = quiet!{[' ' | '\n' | '\t']}
        rule _ () = whitespace()*
        rule __ () = whitespace()+
        rule alphanumeric() -> String
            = s:$['a'..='z' | 'A'..='Z' | '0'..='9'] { s.to_string() }

        rule variable_name() -> String
            = name:$((alphanumeric() / "'" / "*")+) {?
                if is_reserved(name) {
                    Err("Keywords cannot be used as identifiers")
                } else {
                    Ok(name.to_string())
                }
            }

        rule identifier() -> Ast
            = name:variable_name() { Ast::Variable(name) }

        rule number() -> Ast
            = n:$(['0'..='9']+) { Ast::FloatLiteral(n.parse().unwrap()) }

        rule escape() -> String
            = "\\" character:$("\"" / "/" / "n" / "r" / "t") {
                match character {
                    "n" => "\n",
                    "r" => "\r",
                    "t" => "\t",
                    _ => character,
                }.to_string()
            }

        rule string() -> Ast
            = "\"" string:$((!['"' | '\\'] [_] / escape())*) "\"" { Ast::StringLiteral(string.to_string()) }

        rule assignment() -> (String, Vec<String>, Ast)
            = name:variable_name() whitespace()* params:(variable_name() ** (whitespace()*)) whitespace()* "=" whitespace()* value:expression() { (name, params, value) }

        rule lambda() -> Ast
            = "\\" whitespace()* args:(variable_name() ** (whitespace()*)) whitespace()* "->" whitespace()* body:expression() {
                Ast::lambda_chain(body, args)
             }

        rule let_expr() -> Ast
            = "let" whitespace()+ value:assignment() "in" whitespace()+ body:expression() {
                Ast::new_let(value.0, Ast::lambda_chain(value.2, value.1), body)
             }

        rule if_expr() -> Ast
            = "if" whitespace()+ condition:expression() "then" whitespace()+ left:expression() "else" whitespace()+ right:expression() { Ast::new_if(condition, left, right) }

        rule wrapped() -> Ast
            = "(" ret:expression() ")" { ret }

        rule atom() -> Ast
            = ret:(if_expr() / let_expr() / lambda() / number() / string() / identifier() / wrapped()) whitespace()* { ret }

        rule unannotated() -> Ast
            = function:atom() args:(atom() ** (whitespace()*)) whitespace()* { Ast::call_chain(function, args) }

        // Type level syntax
        rule t_identifier() -> Type
            = name:variable_name() {
                let first_char = &name[0..1];

                if first_char == first_char.to_uppercase() {
                    Type::constant(&name[..])
                } else {
                    Type::Variable(VarName { name, kind: Box::new(Type::NoKind) })
                }
             }

        rule t_small() -> Type
            = ret:(t_wrapped() / t_identifier()) whitespace()* { ret }

        rule t_wrapped() -> Type
            = "(" whitespace()* ret:t_atom() ")" { ret }

        rule t_non_lambda() -> Type
            = fun:t_small() args:(t_small()*) { fun.app_chain(args) }

        rule t_lambda() -> Type
            = from:t_non_lambda() "->" whitespace()* to:t_atom() { Type::create_lambda(from, to) }

        rule t_bounded_var() -> SchemeVar
            = "(" _ name:variable_name() _ "::" _  ty:t_atom() _ ")" _ { SchemeVar::Bounded(VarName { kind: Box::new(ty), name }) }

        rule t_unbounded_var() -> SchemeVar
            = name:variable_name() _ { SchemeVar::Unbounded(name) }

        rule t_forall_var() -> SchemeVar
            = t_bounded_var() / t_unbounded_var()

        rule t_forall() -> Type
            = "forall" __ variables:(t_forall_var()+) _ "." _ ty:t_atom() { Type::Scheme { variables, ty: Box::new(ty) } }

        rule t_atom() -> Type
            = ret:(t_forall() / t_lambda() / t_non_lambda()) _ { ret }

        rule annotation() -> Type
            = "::" whitespace()* ret:t_atom() { ret }

        pub rule expression() -> Ast
             = expression:unannotated() annotations:annotation()* { expression.annotate_many(annotations) }
    }
}

pub fn parse_expression(value: &String) -> Result<Ast, peg::error::ParseError<peg::str::LineCol>> {
    parse::expression(value)
}
