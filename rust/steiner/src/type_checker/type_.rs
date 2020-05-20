use crate::parser::Ast;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Type {
    Constant(String),
    Lambda(Box<Type>, Box<Type>),
    Variable(String),
}

pub struct Scheme {
    variables: Vec<String>,
    ty: Type,
}

impl Scheme {
    pub fn new(ty: Type, variables: Vec<String>) -> Scheme {
        Scheme { variables, ty }
    }
}

impl Type {
    pub fn create_lambda(from: Type, to: Type) -> Type {
        Type::Lambda(Box::new(from), Box::new(to))
    }

    // Helper to create a constant from any type of string
    pub fn create_constant(name: &str) -> Type {
        let string = String::from(name);

        Type::Constant(string)
    }

    pub fn generalize(self: &Type) -> Scheme {
        Scheme::new(self.clone(), self.clone().freeVariables())
    }

    // Constructors
    pub fn number() -> Type {
        Type::create_constant("Number")
    }

    pub fn boolean() -> Type {
        Type::create_constant("Boolean")
    }
}

#[derive(Debug)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    NotInScope(String),
}

type TypeResult = Result<Type, TypeError>;

type TypeEnv = HashMap<String, Type>;

#[derive(Debug)]
pub struct TypeContext {
    environment: TypeEnv,
    constraints: Vec<(Type, Type)>,
    next_id: u32,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            environment: TypeEnv::new(),
            constraints: Vec::new(),
            next_id: 0,
        }
    }

    pub fn create_constraint(self: &mut TypeContext, from: &Type, to: &Type) -> () {
        self.constraints.push((from.clone(), to.clone()))
    }

    pub fn get_id(self: &mut TypeContext) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    pub fn fresh(self: &mut TypeContext) -> Type {
        let id = self.get_id();
        Type::Variable(format!("t{}", id))
    }

    pub fn unify(from: TypeContext, to: TypeContext) -> Substitution {
        HashMap::new()
    }
}

pub fn infer(expression: Ast, context: &mut TypeContext) -> TypeResult {
    match expression {
        Ast::FloatLiteral(_) => Ok(Type::number()),
        Ast::Annotation(annotated, annotation) => {
            let inferred = infer(*annotated, context)?;

            context.create_constraint(&annotation, &inferred);

            Ok(annotation)
        }
        Ast::If(condition, right, left) => {
            let type_condition = infer(*condition, context)?;
            let type_right = infer(*right, context)?;
            let type_left = infer(*left, context)?;
            context.create_constraint(&type_condition, &Type::boolean());
            context.create_constraint(&type_left, &type_right);

            Ok(type_right)
        }
        Ast::Variable(name) => {
            let string = String::from_utf8(name.to_vec()).unwrap();
            match context.environment.get(&string) {
                Some(result) => Ok(result.clone()),
                None => Err(TypeError::NotInScope(string)),
            }
        }
        Ast::FunctionCall(function, argument) => {
            let return_type = context.fresh();
            let function_type = infer(*function, context)?;
            let argument_type = infer(*argument, context)?;

            context.create_constraint(
                &function_type,
                &Type::create_lambda(argument_type, return_type.clone()),
            );

            Ok(return_type)
        }
        _ => todo!(),
    }
}

type Substitution = HashMap<String, Type>;

pub trait Substituable {
    fn freeVariables(self: Self) -> Vec<String>;
    fn applySubstitution(self: Self, substitution: &Substitution) -> Self;
}

impl Substituable for Type {
    fn freeVariables(self: Type) -> Vec<String> {
        match self {
            Type::Constant(_) => Vec::new(),
            Type::Variable(name) => vec![name],
            Type::Lambda(from, to) => {
                let mut left_result = from.freeVariables();
                left_result.extend(to.freeVariables());
                left_result
            }
        }
    }

    fn applySubstitution(self: Type, substitution: &Substitution) -> Type {
        match &self {
            Type::Constant(_) => self,
            Type::Variable(name) => match substitution.get(name) {
                Some(new_type) => new_type.clone(),
                None => self,
            },
            Type::Lambda(from, to) => Type::create_lambda(
                from.clone().applySubstitution(substitution),
                to.clone().applySubstitution(substitution),
            ),
        }
    }
}
