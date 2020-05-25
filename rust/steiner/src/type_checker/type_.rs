use crate::parser::Ast;
use im::{hashset, HashSet};
use std::cmp::max;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    Star,
    KFun(Box<Kind>, Box<Kind>),
    KVariable(String),
}

impl Kind {
    // Constructors
    pub fn make_function_kind(from: Kind, to: Kind) -> Kind {
        Kind::KFun(Box::new(from), Box::new(to))
    }

    // Check if a kind contains a reference to itself
    pub fn is_recursive(self: &Kind, name: &String) -> bool {
        match self {
            Kind::Star => false,
            Kind::KFun(from, to) => from.is_recursive(name) || to.is_recursive(name),
            Kind::KVariable(var_name) => var_name == name,
        }
    }
}

impl Display for Kind {
    fn fmt(self: &Kind, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::KFun(from, to) => {
                if let Kind::KFun(_, _) = **from {
                    write!(f, "({}) -> {}", from, to)
                } else {
                    write!(f, "{} -> {}", from, to)
                }
            }
            Kind::KVariable(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarName {
    pub name: String,
    pub kind: Kind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constructor(VarName),
    TApply(Box<Type>, Box<Type>),
    Variable(VarName),
    UnkindedVariable(String),
}

impl Type {
    // Constructors and stuff
    pub fn lambda() -> Type {
        Type::Constructor(VarName {
            name: "Function".to_string(),
            kind: Kind::make_function_kind(
                Kind::Star,
                Kind::make_function_kind(Kind::Star, Kind::Star),
            ),
        })
    }

    // Chain a bunch of type applications together
    pub fn app_chain(self: Type, args: Vec<Type>) -> Type {
        let mut result = self;

        for arg in args {
            result = result.apply(arg)
        }

        result
    }

    pub fn apply(self: &Type, other: Type) -> Type {
        Type::TApply(Box::new(self.clone()), Box::new(other))
    }

    #[inline]
    pub fn create_lambda(from: Type, to: Type) -> Type {
        Type::lambda().apply(from).apply(to)
    }

    #[inline]
    pub fn constant(name: &str) -> Type {
        Type::Constructor(VarName {
            name: String::from(name),
            kind: Kind::Star,
        })
    }

    #[inline]
    pub fn number() -> Type {
        Type::constant("Number")
    }

    #[inline]
    pub fn string() -> Type {
        Type::constant("String")
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
        self.free_variables()
            .iter()
            .find(|VarName { name, kind: _ }| name == variable)
            .is_some()
    }

    pub fn generalize(self: &Type, context: &TypeContext) -> Scheme {
        let quantifiers = self
            .clone()
            .free_variables()
            .iter()
            .filter(|variable| context.environment.contains_key(&variable.name))
            .map(Clone::clone)
            .collect();
        Scheme::new(self.clone(), quantifiers)
    }

    // Check if a type is a function
    pub fn unwrap_function(self: &Type) -> Option<(Type, Type)> {
        if let Type::TApply(first, to) = self {
            if let Type::TApply(fun, from) = *first.clone() {
                if let Type::Constructor(VarName { name, kind: _ }) = *fun {
                    if name == "Function".to_string() {
                        Some((*from, *to.clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl Display for Type {
    fn fmt(self: &Type, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Variable(VarName { name, kind }) => write!(f, "({} :: {})", name, kind),
            ty if ty.unwrap_function().is_some() => {
                let (from, to) = ty.unwrap_function().unwrap();

                if from.unwrap_function().is_some() {
                    write!(f, "({}) -> {}", from, to)
                } else {
                    write!(f, "{} -> {}", from, to)
                }
            }
            Type::TApply(fun, input) => {
                if let Type::TApply(_, _) = **input {
                    write!(f, "{} ({})", fun, input)
                } else {
                    write!(f, "{} {}", fun, input)
                }
            }
            Type::Constructor(VarName { name, kind }) => write!(f, "({} :: {})", name, kind),
            Type::UnkindedVariable(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scheme {
    variables: Vec<VarName>,
    ty: Type,
}

impl Scheme {
    // Constructors
    pub fn new(ty: Type, variables: Vec<VarName>) -> Scheme {
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
        let new_variables = self
            .variables
            .into_iter()
            .map(|var| (var.name, context.fresh(var.kind)));
        let substitution = new_variables.collect();

        self.ty.apply_substitution(&substitution)
    }
}

#[derive(Debug)]
pub enum TypeError {
    TypeMismatch(Type, Type),
    KindMismatch(Kind, Kind),
    NotInScope(String),
    RecursiveType(String, Type),
    RecursiveKind(String, Kind),
    // This uses Boxes so I don't have to do some random unwrapping in the unify_many function
    DifferentLengths(Vec<Type>, Vec<Type>),
}

impl Display for TypeError {
    fn fmt(self: &TypeError, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::TypeMismatch(t1, t2) => {
                write!(f, "Cannot match type\n    {}\nwith type\n    {}", t1, t2)
            }
            TypeError::KindMismatch(kind1, kind2) => write!(
                f,
                "Cannot match kind\n    {}\nwith kind\n    {}",
                kind1, kind2
            ),
            TypeError::NotInScope(name) => write!(f, "Variable {} is not in scope", name),
            TypeError::RecursiveType(name, ty) => write!(
                f,
                "Type \n    {} = {}\ncontains references to itself",
                name, ty
            ),
            TypeError::RecursiveKind(name, kind) => write!(
                f,
                "Kind \n    {} = {}\n contains a reference to itself",
                name, kind
            ),
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
    kind_constraints: Vec<(Kind, Kind)>,
    next_id: u32,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            environment: TypeEnv::new(),
            constraints: Vec::new(),
            kind_constraints: Vec::new(),
            next_id: 0,
        }
    }

    // Create a constraint requiring 2 types to be equal
    pub fn create_constraint(self: &mut TypeContext, from: &Type, to: &Type) -> () {
        self.constraints.push((from.clone(), to.clone()))
    }

    // Same but for kinds
    pub fn create_kind_constraint(self: &mut TypeContext, from: &Kind, to: &Kind) -> () {
        self.kind_constraints.push((from.clone(), to.clone()))
    }

    // Generate a new unique id
    pub fn get_id(self: &mut TypeContext) -> u32 {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    // Generate a new unique type variable
    pub fn fresh(self: &mut TypeContext, kind: Kind) -> Type {
        let id = self.get_id();
        Type::Variable(VarName {
            name: format!("t{}", id),
            kind,
        })
    }

    // Generate a new unique kind variable
    pub fn fresh_kind(self: &mut TypeContext) -> Kind {
        let id = self.get_id();
        Kind::KVariable(format!("k{}", id))
    }

    // Take a type with unkinded parts and generate fresh kinds for them
    pub fn kind_unkinded_type(self: &mut TypeContext, ty: Type) -> Type {
        match ty {
            Type::UnkindedVariable(name) => Type::Variable(VarName {
                name,
                kind: self.fresh_kind(),
            }),
            Type::TApply(fun, input) => self
                .kind_unkinded_type(*fun)
                .apply(self.kind_unkinded_type(*input)),
            other => other,
        }
    }

    pub fn solve_constraints_with_subst(
        self: &mut TypeContext,
        constraints: &Vec<(Type, Type)>,
        substitution: Substitution,
    ) -> TypeResult<Substitution> {
        match &constraints[..] {
            [] => Ok(substitution),
            [(left, right), ..] => {
                let new_subst =
                    merge_substitutions(self.unify(left.clone(), right.clone())?, substitution);
                let constraints = Vec::from_iter(constraints[1..].iter().map(|(left, right)| {
                    (
                        left.clone().apply_substitution(&new_subst),
                        right.clone().apply_substitution(&new_subst),
                    )
                }));
                self.solve_constraints_with_subst(&constraints, new_subst)
            }
        }
    }

    pub fn solve_constraints(self: &mut TypeContext) -> TypeResult<Substitution> {
        self.solve_constraints_with_subst(&self.constraints.clone(), Substitution::new())
    }

    // Create a new context based on a new variable
    pub fn create_closure(self: &TypeContext, name: String, scheme: Scheme) -> TypeContext {
        let mut context = self.clone();

        context.environment.insert(name, scheme);

        context
    }

    // Infer a expression inside a closure
    pub fn infer_with(
        self: &mut TypeContext,
        name: String,
        scheme: Scheme,
        ast: Ast,
    ) -> TypeResult {
        let mut new_ctx = self.create_closure(name, scheme);
        let result = new_ctx.infer(ast);

        self.sync(new_ctx);

        result
    }

    // copy stuff over from another context
    pub fn sync(self: &mut TypeContext, other: TypeContext) -> () {
        self.constraints.extend(other.constraints);
        self.next_id = max(other.next_id, self.next_id);
    }

    // Infer the type of an expression
    pub fn infer(self: &mut TypeContext, expression: Ast) -> TypeResult {
        match expression {
            Ast::FloatLiteral(_) => Ok(Type::number()),
            Ast::StringLiteral(_) => Ok(Type::string()),
            Ast::Annotation(annotated, annotation) => {
                let inferred = self.infer(*annotated)?;
                let kinded = self.kind_unkinded_type(annotation);
                let annotation_kind = self.kind(&kinded);

                // The annotation always needs to have kind Star
                self.create_kind_constraint(&Kind::Star, &annotation_kind);

                self.create_constraint(&kinded, &inferred);

                Ok(kinded)
            }
            Ast::If(condition, right, left) => {
                let type_condition = self.infer(*condition)?;
                let type_right = self.infer(*right)?;
                let type_left = self.infer(*left)?;
                self.create_constraint(&type_condition, &Type::boolean());
                self.create_constraint(&type_left, &type_right);

                Ok(type_right)
            }
            Ast::Variable(name) => match self.environment.get(&name) {
                Some(result) => Ok(result.clone().instantiate(self)),
                None => Err(TypeError::NotInScope(name)),
            },
            Ast::FunctionCall(function, argument) => {
                let return_type = self.fresh(Kind::Star);
                let function_type = self.infer(*function)?;
                let argument_type = self.infer(*argument)?;

                self.create_constraint(
                    &function_type,
                    &Type::create_lambda(argument_type, return_type.clone()),
                );

                Ok(return_type)
            }
            Ast::Lambda(argument, body) => {
                let arg_type = self.fresh(Kind::Star);
                let return_type = self.infer_with(argument, arg_type.to_scheme(), *body)?;

                Ok(Type::create_lambda(arg_type, return_type))
            }
            Ast::Let(name, value, body) => {
                let mut value_ctx = self.clone();
                value_ctx.constraints = Vec::new();
                let value_type = value_ctx.infer(*value)?;
                let substitution = value_ctx.solve_constraints()?;

                self.with_substitution(&substitution);

                let scheme = value_type
                    .apply_substitution(&substitution)
                    .generalize(self);

                self.sync(value_ctx);
                self.infer_with(name, scheme, *body)
            }
        }
    }

    // Applies a substitution on the current environment
    pub fn with_substitution(self: &mut Self, substitution: &Substitution) -> () {
        self.environment = self.environment.clone().apply_substitution(substitution);
    }

    pub fn kind(self: &mut TypeContext, ty: &Type) -> Kind {
        match ty {
            Type::Constructor(VarName { kind, name: _ }) => kind.clone(),
            Type::Variable(VarName { kind, name: _ }) => kind.clone(),
            Type::TApply(func, _) => {
                let func_kind = self.kind(func);
                if let Kind::KFun(_, to) = func_kind {
                    *to
                } else {
                    let input_kind = self.fresh_kind();
                    let output_kind = self.fresh_kind();

                    let fresh_func_kind = Kind::make_function_kind(input_kind, output_kind.clone());

                    self.create_kind_constraint(&func_kind, &fresh_func_kind);

                    output_kind
                }
            }
            Type::UnkindedVariable(name) => panic!("Type variable {} has no kind info", name),
        }
    }

    // Constraint a type application to have the correct kinds. Assumes everything has kind data.
    pub fn constraint_type_application(self: &mut Self, left: &Type, right: &Type) -> () {
        let ret = self.fresh_kind();
        let left_kind = self.kind(left);
        let right_kind = self.kind(right);

        self.create_kind_constraint(&left_kind, &Kind::make_function_kind(right_kind, ret));
    }

    pub fn unify(self: &mut Self, left: Type, right: Type) -> TypeResult<Substitution> {
        match (&left, &right) {
            (
                Type::Constructor(VarName {
                    name: name_left,
                    kind: kind_left,
                }),
                Type::Constructor(VarName {
                    name: name_right,
                    kind: kind_right,
                }),
            ) if name_right == name_left => {
                self.create_kind_constraint(kind_left, kind_right);
                Ok(Substitution::new())
            }
            (Type::Variable(name), right) => self.bind_type_variable(name.clone(), right.clone()),
            (left, Type::Variable(name)) => self.bind_type_variable(name.clone(), left.clone()),
            (Type::TApply(fun_left, input_left), Type::TApply(fun_right, input_right)) => {
                self.constraint_type_application(fun_left, input_left);
                self.constraint_type_application(fun_right, input_right);
                self.unify_many(
                    vec![*fun_left.clone(), *input_left.clone()],
                    vec![*fun_right.clone(), *input_right.clone()],
                )
            }
            (left, right) => Err(TypeError::TypeMismatch(left.clone(), right.clone())),
        }
    }

    // Unify 2 vectors of types 1 by 1
    pub fn unify_many(
        self: &mut Self,
        types1: Vec<Type>,
        types2: Vec<Type>,
    ) -> TypeResult<Substitution> {
        match (types1.split_first(), types2.split_first()) {
            (None, None) => Ok(Substitution::new()),
            (Some((left, types1)), Some((right, types2))) => {
                let substitution = self.unify(left.clone(), right.clone())?;
                let other_substitution = self.unify_many(
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

    // Bind a type variable to a type
    pub fn bind_type_variable(
        self: &mut Self,
        name: VarName,
        ty: Type,
    ) -> TypeResult<Substitution> {
        match &ty {
            Type::Variable(var_name) if *var_name.name == name.name => {
                self.create_kind_constraint(&var_name.kind, &name.kind);
                Ok(Substitution::new())
            }
            _ => {
                if ty.is_recursive(&name.name) {
                    Err(TypeError::RecursiveType(name.name, ty))
                } else {
                    let map = Substitution::new().update(name.name, ty.clone());
                    let kind = self.kind(&ty);
                    self.create_kind_constraint(&name.kind, &kind);

                    Ok(map)
                }
            }
        }
    }

    fn solve_kind_constraints_with_subst(
        self: &mut TypeContext,
        constraints: &Vec<(Kind, Kind)>,
        substitution: KindSubstitution,
    ) -> TypeResult<KindSubstitution> {
        match &constraints[..] {
            [] => Ok(substitution),
            [(left, right), ..] => {
                let new_subst = merge_kind_substitutions(
                    unify_kinds(left.clone(), right.clone())?,
                    substitution,
                );
                let constraints = Vec::from_iter(constraints[1..].iter().map(|(left, right)| {
                    (
                        left.clone().apply_kind_subst(&new_subst),
                        right.clone().apply_kind_subst(&new_subst),
                    )
                }));
                self.solve_kind_constraints_with_subst(&constraints, new_subst)
            }
        }
    }

    pub fn solve_kind_constraints(self: &mut TypeContext) -> TypeResult<KindSubstitution> {
        self.solve_kind_constraints_with_subst(
            &self.kind_constraints.clone(),
            KindSubstitution::new(),
        )
    }
}

pub type Substitution = im::HashMap<String, Type>;

fn merge_substitutions(subst1: Substitution, subst2: Substitution) -> Substitution {
    subst2.apply_substitution(&subst1).union(subst1)
}

pub trait Substituable {
    fn free_variables(self: &Self) -> HashSet<VarName>;
    fn apply_substitution(self: Self, substitution: &Substitution) -> Self;
}

impl Substituable for Vec<Type> {
    fn free_variables(self: &Self) -> HashSet<VarName> {
        self.iter()
            .flat_map(|ty| ty.clone().free_variables())
            .collect()
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> Self {
        let mut result = Vec::new();

        for ty in self {
            result.push(ty.clone().apply_substitution(substitution))
        }
        result
    }
}

impl Substituable for Substitution {
    fn free_variables(self: &Substitution) -> HashSet<VarName> {
        self.values()
            .flat_map(|ty| ty.clone().free_variables())
            .collect()
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> Substitution {
        self.iter()
            .map(|(key, ty)| (key.clone(), ty.clone().apply_substitution(substitution)))
            .collect()
    }
}

impl Substituable for TypeEnv {
    fn free_variables(self: &Self) -> HashSet<VarName> {
        self.values()
            .flat_map(|scheme| scheme.ty.clone().free_variables())
            .collect()
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> TypeEnv {
        HashMap::from_iter(
            self.iter().map(|(key, scheme)| {
                (key.clone(), scheme.clone().apply_substitution(substitution))
            }),
        )
    }
}

impl Substituable for TypeContext {
    fn free_variables(self: &Self) -> HashSet<VarName> {
        self.environment.free_variables()
    }

    fn apply_substitution(self: Self, substitution: &Substitution) -> TypeContext {
        TypeContext {
            environment: self.environment.apply_substitution(substitution),
            ..self
        }
    }
}

impl Substituable for Scheme {
    fn free_variables(self: &Self) -> HashSet<VarName> {
        self.ty
            .free_variables()
            .iter()
            .filter(|v| !self.variables.contains(v))
            .map(Clone::clone)
            .collect()
    }
    fn apply_substitution(self: Self, substitution: &Substitution) -> Self {
        let new_subst = self
            .variables
            .iter()
            .fold(substitution.clone(), |acc, current| {
                acc.without(&current.name)
            });
        Scheme {
            ty: self.ty.apply_substitution(&new_subst),
            ..self
        }
    }
}

impl Substituable for Type {
    fn free_variables(self: &Type) -> HashSet<VarName> {
        match self {
            Type::Variable(name) => hashset![name.clone()],
            Type::TApply(fun, input) => fun.free_variables().union(input.free_variables()),
            _ => HashSet::new(),
        }
    }

    fn apply_substitution(self: Type, substitution: &Substitution) -> Type {
        match &self {
            Type::Variable(VarName { name, kind: _ }) => match substitution.get(name) {
                Some(new_type) => new_type.clone(),
                None => self,
            },
            Type::TApply(fun, input) => (*fun.clone())
                .apply_substitution(substitution)
                .apply(input.clone().apply_substitution(substitution)),
            _ => self,
        }
    }
}

// STUFF RELATED TO KIND SUBSTITUTIONS
type KindSubstitution = im::HashMap<String, Kind>;

pub trait Kindable {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Self;
}

impl Kindable for KindSubstitution {
    fn apply_kind_subst(self: Self, substitution: &KindSubstitution) -> KindSubstitution {
        self.iter()
            .map(|(key, ty)| (key.clone(), ty.clone().apply_kind_subst(substitution)))
            .collect()
    }
}

impl Kindable for Vec<(Kind, Kind)> {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Vec<(Kind, Kind)> {
        self.iter()
            .map(|(left, right)| {
                (
                    left.clone().apply_kind_subst(subst),
                    right.clone().apply_kind_subst(subst),
                )
            })
            .collect()
    }
}

impl Kindable for Kind {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Self {
        match &self {
            Kind::KVariable(name) => match subst.get(name) {
                Some(res) => res.clone(),
                None => self.clone(),
            },
            Kind::KFun(from, to) => Kind::make_function_kind(
                from.clone().apply_kind_subst(subst),
                to.clone().apply_kind_subst(subst),
            ),
            Kind::Star => Kind::Star,
        }
    }
}

impl Kindable for VarName {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Self {
        VarName {
            kind: self.kind.apply_kind_subst(subst),
            ..self
        }
    }
}

impl Kindable for Type {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Self {
        match self {
            Type::Constructor(var) => Type::Constructor(var.apply_kind_subst(subst)),
            Type::Variable(var) => Type::Variable(var.apply_kind_subst(subst)),
            Type::TApply(fun, input) => fun
                .apply_kind_subst(subst)
                .apply(input.apply_kind_subst(subst)),
            other => other,
        }
    }
}

impl Kindable for Substitution {
    fn apply_kind_subst(self: Self, subst: &KindSubstitution) -> Self {
        self.iter()
            .map(|(key, value)| (key.clone(), value.clone().apply_kind_subst(subst)))
            .collect()
    }
}

fn unify_kinds(left: Kind, right: Kind) -> TypeResult<KindSubstitution> {
    match (left, right) {
        (left, right) if left == right => Ok(KindSubstitution::new()),
        (Kind::KVariable(name), other) => bind_kind_variable(name, other),
        (other, Kind::KVariable(name)) => bind_kind_variable(name, other),
        (Kind::KFun(from_left, to_left), Kind::KFun(from_right, to_right)) => {
            let subst = unify_kinds(*from_left, *from_right)?;
            let subst2 = unify_kinds(
                to_left.apply_kind_subst(&subst),
                to_right.apply_kind_subst(&subst),
            )?;

            Ok(merge_kind_substitutions(subst2, subst))
        }
        (left, right) => Err(TypeError::KindMismatch(left, right)),
    }
}

fn merge_kind_substitutions(
    subst1: KindSubstitution,
    subst2: KindSubstitution,
) -> KindSubstitution {
    subst2.apply_kind_subst(&subst1).union(subst1)
}

fn bind_kind_variable(name: String, kind: Kind) -> TypeResult<KindSubstitution> {
    match &kind {
        Kind::KVariable(var_name) if *var_name == name => Ok(KindSubstitution::new()),
        _ => {
            if kind.is_recursive(&name) {
                Err(TypeError::RecursiveKind(name, kind))
            } else {
                let map = KindSubstitution::new().update(name, kind);

                Ok(map)
            }
        }
    }
}

// ACTUAL FUNCTION FOR GETTING THE TYPE OF AN EXPRESSION
pub fn get_type_of(expression: Ast) -> TypeResult {
    let mut context = TypeContext::new();
    let resulting_type = context.infer(expression)?;
    let subst = context.solve_constraints()?;
    let kind_subst = context.solve_kind_constraints()?;
    let final_subst = subst.apply_kind_subst(&kind_subst);

    Ok(resulting_type
        .apply_kind_subst(&kind_subst)
        .apply_substitution(&final_subst))
}
