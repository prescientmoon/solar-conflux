package metafurl

// {{{ Syntax primitives
File :: struct {
	source: string,
}

Source_Pos :: struct {
	index: uint,
	line:  uint,
	col:   uint,
}

Source_Span :: struct {
	file:   ^File,
	from:   Source_Pos,
	length: uint,
}

Name :: struct {
	span: Source_Span,
}

Qualified_Name :: struct {
	path: []Name,
}
// }}}
// {{{ Types
Rigid_Id :: distinct uint
Rigid :: struct {
	id:   Rigid_Id,
	type: ^Type,
}

Flex_Id :: distinct uint
Flex :: struct {
	id:   Flex_Id,
	type: ^Type,
}

Type_Unit :: struct {}
Type_Type :: struct {}
Type_Array :: struct {
	length: uint,
	inner:  ^Type,
}

Type_Projection :: struct {
	inner:      ^Type,
	projection: Name,
}

Type_Proc :: struct {
	args:      []Type,
	return_ty: ^Type,
}

Type_Quotient :: struct {
	ty:    ^Type,
	name:  Qualified_Name,
	value: ^Type, // TODO: this could be an expression
}

Struct :: struct {
	names: []Name,
	types: []Type,
}

Type :: union {
	Flex,
	Rigid,
	Type_Unit,
	Type_Type,
	Type_Array,
	Type_Projection,
	Type_Proc,
	Type_Quotient,
	Qualified_Name,
	Struct,
}
// }}}
// {{{ Expressions
Expr_Unit :: struct {}

Expr_Projection :: struct {
	inner:      ^Expr,
	projection: Name,
}

Expr_Call :: struct {
	callee: Qualified_Name,
	spine:  []Expr,
}

Unary_Operator :: enum {
	Plus, // +
	Minus, // -
	Not, // !
	BitwiseNot, // ~
}

Expr_Unary :: struct {
	operator: Unary_Operator,
	inner:    ^Expr,
}

Binary_Operator :: enum {
	DoubleEqual, // ==
	NotEqual, // !=
	Plus, // +
	Minus, // -
	Multiply, // *
	Divide, // /
	And, // &
	Or, // |
	Xor, // ^
	Meet, // /\
	Join, // \/
	LeftShift, // <<
	RightShift, // >>
	GreaterThan, // >
	LesserThan, // <
	GreaterOrEqual, // >=
	LesserOrEqual, // <=
}

Expr_Binary :: struct {
	lhs:      ^Expr,
	operator: Binary_Operator,
	rhs:      ^Expr,
}

Expr_Ternary :: struct {
	cond:     ^Expr,
	if_true:  ^Expr,
	if_false: ^Expr,
}

Expr :: union {
	Qualified_Name,
	Expr_Unit,
	bool,
	i64,
	f64,
	Expr_Projection,
	Expr_Unary,
	Expr_Binary,
	Expr_Ternary,
}
// }}}
// {{{ Statements
Statement_Simple :: enum {
	Discard,
	Break,
	Continue,
}

Statement_Return :: struct {
	expr: Expr,
}

Statement_For :: struct {
	init: ^Statement,
	cond: ^Statement,
	next: ^Statement,
	body: ^Statement,
}

Statement_If :: struct {
	cond:     Expr,
	if_true:  ^Statement,
	if_false: ^Statement,
}

Statement_Block :: struct {
	steps: []Statement,
}

Statement_Declaration :: struct {
	name:  Name,
	value: Expr,
	type:  Type,
}

Statement_Assignment :: struct {
	assign_to: Expr,
	value:     Expr,
}

Statement :: union {
	Statement_Simple,
	Statement_Return,
	Statement_For,
	Statement_If,
	Statement_Block,
	Statement_Declaration,
}
// }}}
// {{{ Procedures
Proc :: struct {
	names:          []Name,
	args:           []Type,
	return_ty:      Type,
	implementation: Statement,
}
// }}}
// {{{ Theories
Theory :: struct {
	imports:       []Qualified_Name,
	names:         []Qualified_Name,
	types:         []Type,
	alias_names:   []Qualified_Name,
	alias_targets: []Qualified_Name,
}

Theory_Implementation :: struct {
	inner: Module,
}
// }}}
// {{{ External values
External :: enum {
	Uniform,
	Attribute,
	Varying,
	UniformBuffer,
	Buffer, // SSBO
}
// }}}
// {{{ Declarations & modules
Declaration_Value :: union {
	Qualified_Name,
	Module,
	Type,
	Proc,
	Theory,
	External,
	Rigid,
	Theory_Implementation,
}

Declaration :: struct {
	name:  Qualified_Name,
	type:  Type,
	value: Declaration_Value,
}

Module :: struct {
	imports:      []Qualified_Name,
	declarations: []Declaration,
}
// }}}
