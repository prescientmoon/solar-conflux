use crate::parser::Ast;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone)]
pub struct Scheme {
    variables: Vec<String>,
    ty: Type,
}

impl Scheme {
    // Constructors
    pub fn new(ty: Type, variables: Vec<String>) -> Scheme {
        Scheme { variables, ty }
    }

    #[inline]
    pub fn from_type(ty: &Type) -> Scheme {
        Scheme {
            ty: ty.clone(),
            variables: Vec::new(),
        }
    }

    // The opposite of generalize
    pub fn instantiate(self: Scheme, context: &mut TypeContext) -> Type {
        let new_variables = self.variables.into_iter().map(|var| (var, context.fresh()));
        let substitution = HashMap::from_iter(new_variables);

        self.ty.apply_substitution(&substitution)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constructor { name: String, args: Vec<Box<Type>> },
    Variable(String),
}

impl Type {
    // Constructors and stuff
    #[inline]
    pub fn create_lambda(from: Type, to: Type) -> Type {
        Type::Constructor {
            name: String::from("Function"),
            args: vec![Box::new(from), Box::new(to)],
        }
    }

    #[inline]
    pub fn constant(name: &str) -> Type {
        Type::Constructor {
            name: String::from(name),
            args: Vec::new(),
        }
    }

    #[inline]
    pub fn number() -> Type {
        Type::constant("Number")
    }

    #[inline]
    pub fn boolean() -> Type {
        Type::constant("Boolean")
    }
    // Other helpers and stuff
    #[inline]
    pub fn to_scheme(self: &Type) -> Scheme {
        Scheme::from_type(self)
    }

    // Returns true if the type has a reference to itself
    pub fn is_recursive(self: &Type, variable: &String) -> bool {
        self.free_variables().contains(&variable)
    }

    pub fn generalize(self: &Type) -> Scheme {
        Scheme::new(self.clone(), self.clone().free_variables())
    }
}

#[derive(Debug)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    NotInScope(String),
    RecursiveType(Type),
    // This uses Boxes so I don't have to do some random unwrapping in the unify_many function
    DifferentLengths(Vec<Box<Type>>, Vec<Box<Type>>),
}

impl Display for TypeError {
    fn fmt(self: &TypeError, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::TypeMismatch(t1, t2) => {
                write!(f, "Cannot match type\n    {:?}\nwith type\n    {:?}", t1, t2)
            }
            TypeError::NotInScope(name) => write!(f, "Variable {} is not in scope", name),
            TypeError::RecursiveType(ty) => {
                write!(f, "Type \n{:?}\n    contains references to itself", ty)
            }
            TypeError::DifferentLengths(tys1, tys2) => write!(
                f,
                "Cannot match length {} with {} while trying to unify types\n    {:?}\nwith\n    {:?}",
                tys1.len(),
                tys2.len(),
                tys1,
                tys2
            ),
        }
    }
}

type TypeResult<T = Type> = Result<T, TypeError>;

type TypeEnv = HashMap<String, Scheme>;

#[derive(Debug, Clone)]
pub struct TypeContext {
    environment: TypeEnv,
    constraints: Vec<(Type, Type)>,
    next_id: u32,
    last_substitution: Substitution,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            environment: TypeEnv::new(),
            constraints: Vec::new(),
            next_id: 0,
            last_substitution: Substitution::new(),
        }
    }

    pub fn create_constraint(self: &mut TypeContext, from: &Type, to: &Type) -> () {
        self.constraints.push((from.clone(), to.clone()))
    }

    // Generate a new unique id
    pub fn get_id(self: &mut TypeContext) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    // Generate a new unique type variable
    pub fn fresh(self: &mut TypeContext) -> Type {
        let id = self.get_id();
        Type::Variable(format!("t{}", id))
    }

    pub fn solve_constraints(self: &mut TypeContext) -> TypeResult<Substitution> {
        match &self.constraints[..] {
            [] => Ok(self.last_substitution.clone()),
            [(left, right), ..] => {
                self.last_substitution = merge_substitutions(
                    unify(left.clone(), right.clone())?,
                    self.last_substitution.clone(),
                );
                self.constraints =
                    Vec::from_iter(self.constraints[1..].iter().map(|(left, right)| {
                        (
                            left.clone().apply_substitution(&self.last_substitution),
                            right.clone().apply_substitution(&self.last_substitution),
                        )
                    }));
                self.solve_constraints()
            }
        }
    }

    pub fn infer(self: &mut TypeContext, expression: Ast) -> TypeResult {
        match expression {
            Ast::FloatLiteral(_) => Ok(Type::number()),
            Ast::Annotation(annotated, annotation) => {
                let inferred = self.infer(*annotated)?;

                self.create_constraint(&annotation, &inferred);

                Ok(annotation)
            }
            Ast::If(condition, right, left) => {
                let type_condition = self.infer(*condition)?;
                let type_right = self.infer(*right)?;
                let type_left = self.infer(*left)?;
                self.create_constraint(&type_condition, &Type::boolean());
                self.create_constraint(&type_left, &type_right);

                Ok(type_right)
            }
            Ast::Variable(name) => {
                let string = String::from_utf8(name.to_vec()).unwrap();
                match self.environment.get(&string) {
                    Some(result) => Ok(result.clone().instantiate(self)),
                    None => Err(TypeError::NotInScope(string)),
                }
            }
            Ast::FunctionCall(function, argument) => {
                let return_type = self.fresh();
                let function_type = self.infer(*function)?;
                let argument_type = self.infer(*argument)?;

                self.create_constraint(
                    &function_type,
                    &Type::create_lambda(argument_type, return_type.clone()),
                );

                Ok(return_type)
            }
            Ast::Lambda(argument, body) => {
                let arg_type = self.fresh();
                let mut body_context = self.clone();
                let arg_name = String::from_utf8(argument.to_vec()).unwrap();

                body_context
                    .environment
                    .insert(arg_name, arg_type.to_scheme());

                let return_type = body_context.infer(*body)?;

                self.constraints.append(&mut body_context.constraints);

                Ok(Type::create_lambda(arg_type, return_type))
            }
            _ => todo!(),
        }
    }
}

type Substitution = HashMap<String, Type>;

fn merge_substitutions(subst1: Substitution, subst2: Substitution) -> Substitution {
    let mut combination = subst2.apply_substitution(&subst1);

    combination.extend(subst1);

    combination
}

pub trait Substituable {
    fn free_variables(self: &Self) -> Vec<String>;
    fn apply_substitution(self: Self, substitution: &Substitution) -> Self;
}

impl Substituable for Vec<Box<Type>> {
    fn free_variables(self: &Self) -> Vec<String> {
        Vec::from_iter(self.iter().flat_map(|ty| ty.clone().free_variables()))
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> Self {
        let mut result = Vec::new();

        for ty in self {
            result.push(Box::new(ty.clone().apply_substitution(substitution)))
        }
        result
    }
}

impl Substituable for Substitution {
    fn free_variables(self: &Substitution) -> Vec<String> {
        Vec::from_iter(self.values().flat_map(|ty| ty.clone().free_variables()))
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> Substitution {
        HashMap::from_iter(
            self.iter()
                .map(|(key, ty)| (key.clone(), ty.clone().apply_substitution(substitution))),
        )
    }
}

impl Substituable for Type {
    fn free_variables(self: &Type) -> Vec<String> {
        match self {
            Type::Constructor { args, name: _ } => {
                let mut result = Vec::new();
                for arg in args {
                    result.extend(arg.free_variables())
                }

                result
            }
            Type::Variable(name) => vec![name.clone()],
        }
    }

    fn apply_substitution(self: Type, substitution: &Substitution) -> Type {
        match &self {
            Type::Constructor { name, args } => Type::Constructor {
                name: name.clone(),
                args: Vec::from_iter(
                    args.iter()
                        .map(|t| Box::new(t.clone().apply_substitution(substitution))),
                ),
            },
            Type::Variable(name) => match substitution.get(name) {
                Some(new_type) => new_type.clone(),
                None => self,
            },
        }
    }
}

fn unify(left: Type, right: Type) -> TypeResult<Substitution> {
    match (left, right) {
        (left, right) if right == left => Ok(HashMap::new()),
        (Type::Variable(name), right) => bind_variable(name, right),
        (left, Type::Variable(name)) => bind_variable(name, left),
        (
            Type::Constructor {
                name: name_left,
                args: args_left,
            },
            Type::Constructor {
                name: name_right,
                args: args_right,
            },
        ) if name_left == name_right => unify_many(args_left, args_right),
        (left, right) => Err(TypeError::TypeMismatch(left, right)),
    }
}

fn unify_many(types1: Vec<Box<Type>>, types2: Vec<Box<Type>>) -> TypeResult<Substitution> {
    match (types1.split_first(), types2.split_first()) {
        (None, None) => Ok(Substitution::new()),
        (Some((left, types1)), Some((right, types2))) => {
            let substitution = unify(*left.clone(), *right.clone())?;
            let other_substitution = unify_many(
                Vec::from(types1).apply_substitution(&substitution),
                Vec::from(types2).apply_substitution(&substitution),
            )?;
            Ok(merge_substitutions(other_substitution, substitution))
        }
        _ => Err(TypeError::DifferentLengths(
            types1.to_vec(),
            types2.to_vec(),
        )),
    }
}

fn bind_variable(name: String, ty: Type) -> TypeResult<Substitution> {
    match &ty {
        Type::Variable(var_name) if *var_name == name => Ok(Substitution::new()),
        _ => {
            if ty.is_recursive(&name) {
                Err(TypeError::RecursiveType(ty))
            } else {
                let mut map = HashMap::new();

                map.insert(name, ty);

                Ok(map)
            }
        }
    }
}

pub fn get_type_of(expression: Ast) -> TypeResult {
    let mut context = TypeContext::new();
    let resulting_type = context.infer(expression)?;
    let subst = context.solve_constraints()?;

    println!("{:?}", subst);

    Ok(resulting_type.apply_substitution(&subst))
}
