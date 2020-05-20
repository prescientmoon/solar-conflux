use crate::parser::Ast;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constant(String),
    Lambda(Box<Type>, Box<Type>),
    Variable(String),
}

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

impl Type {
    // Constructors and stuff
    pub fn create_lambda(from: Type, to: Type) -> Type {
        Type::Lambda(Box::new(from), Box::new(to))
    }

    #[inline]
    pub fn to_scheme(self: &Type) -> Scheme {
        Scheme::from_type(self)
    }

    // Helper to create a constant from any type of string
    pub fn create_constant(name: &str) -> Type {
        let string = String::from(name);

        Type::Constant(string)
    }

    // Returns true if the type has a reference to itself
    pub fn isRecursive(self: &Type, variable: &String) -> bool {
        self.free_variables().contains(&variable)
    }

    pub fn generalize(self: &Type) -> Scheme {
        Scheme::new(self.clone(), self.clone().free_variables())
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
    RecursiveType(Type),
    DifferentLengths(Vec<Type>, Vec<Type>),
}

type TypeResult<T = Type> = Result<T, TypeError>;

type TypeEnv = HashMap<String, Scheme>;

#[derive(Debug, Clone)]
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

fn mergeSubstitutions(subst1: Substitution, subst2: Substitution) -> Substitution {
    let mut combination = subst2.apply_substitution(&subst1);

    combination.extend(subst1);

    combination
}

pub trait Substituable {
    fn free_variables(self: &Self) -> Vec<String>;
    fn apply_substitution(self: Self, substitution: &Substitution) -> Self;
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
            Type::Constant(_) => Vec::new(),
            Type::Variable(name) => vec![name.clone()],
            Type::Lambda(from, to) => {
                let mut left_result = from.free_variables();
                left_result.extend(to.free_variables());
                left_result
            }
        }
    }

    fn apply_substitution(self: Type, substitution: &Substitution) -> Type {
        match &self {
            Type::Constant(_) => self,
            Type::Variable(name) => match substitution.get(name) {
                Some(new_type) => new_type.clone(),
                None => self,
            },
            Type::Lambda(from, to) => Type::create_lambda(
                from.clone().apply_substitution(substitution),
                to.clone().apply_substitution(substitution),
            ),
        }
    }
}

fn unify(left: Type, right: Type) -> Substitution {
    match (left, right) {
        (left, right) if right == left => HashMap::new(),
        _ => todo!(),
    }
}

fn unifyMany<'a>(types1: &'a [Type], types2: &'a [Type]) -> TypeResult<Substitution> {
    match (types1, types2) {
        ([], []) => Ok(Substitution::new()),
        ([left, ..], [right, ..]) => {
            let substitution = unify(left.clone(), right.clone());
            let other_substitution = unifyMany(&types1[1..], &types2[1..])?;
            Ok(mergeSubstitutions(other_substitution, substitution))
        }
        _ => Err(TypeError::DifferentLengths(
            types1.to_vec(),
            types2.to_vec(),
        )),
    }
}

fn bindVariable(name: String, ty: Type) -> TypeResult<Substitution> {
    match &ty {
        Type::Variable(var_name) if *var_name == name => Ok(Substitution::new()),
        _ => {
            if ty.isRecursive(&name) {
                Err(TypeError::RecursiveType(ty))
            } else {
                let mut map = HashMap::new();

                map.insert(name, ty);

                Ok(map)
            }
        }
    }
}

pub fn getTypeOf(expression: Ast) -> TypeResult {
    let mut context = TypeContext::new();
    let resultingType = context.infer(expression);

    resultingType
}
