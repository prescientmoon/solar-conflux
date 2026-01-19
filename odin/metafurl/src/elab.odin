package metafurl

// {{{ Syntax primitives
Name :: struct {
	span: Source_Span,
}

ast__Qualified_Name :: struct {
	path: []Name,
}
// }}}
// {{{ Expressions
Rigid_Id :: distinct uint
Rigid :: struct {
	id:   Rigid_Id,
	type: ^ast__Type,
}

Flex_Id :: distinct uint
Flex :: struct {
	id:   Flex_Id,
	type: ^ast__Type,
}

ast__Expr_Unit :: struct {}
ast__Type_Unit :: struct {}
ast__Type_Type :: struct {}
ast__Type_Array :: struct {
	length: uint,
	inner:  ^ast__Type,
}

ast__Projection :: struct {
	inner:      ^ast__Expr,
	projection: Name,
}

ast__Type_Quotient :: struct {
	theory: ^ast__Type,
	name:   ast__Qualified_Name,
	value:  ^ast__Expr,
}

ast__Type_Struct :: struct {
	names: []Name,
	types: []ast__Type,
}

ast__Expr_Call :: struct {
	callee: ast__Qualified_Name,
	spine:  []ast__Expr,
}

ast__Unary_Operator :: enum {
	Plus, // +
	Minus, // -
	Not, // !
	BitwiseNot, // ~
}

ast__Expr_Unary :: struct {
	operator: ast__Unary_Operator,
	inner:    ^ast__Expr,
}

ast__Binary_Operator :: enum {
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

ast__Expr_Binary :: struct {
	lhs:      ^ast__Expr,
	operator: ast__Binary_Operator,
	rhs:      ^ast__Expr,
}

ast__Expr_Ternary :: struct {
	cond:     ^ast__Expr,
	if_true:  ^ast__Expr,
	if_false: ^ast__Expr,
}

ast__Expr :: union {
	Flex,
	Rigid,
	ast__External,
	ast__Projection,
	ast__Qualified_Name,
	ast__Expr_Unit,
	ast__Type_Unit,
	ast__Expr_Call,
	ast__Expr_Proc,
	ast__Type_Proc,
	ast__Type_Type,
	ast__Type_Array,
	ast__Type_Quotient,
	ast__Type_Struct,
	ast__Expr_Unary,
	ast__Expr_Binary,
	ast__Expr_Ternary,
	ast__Module,
	ast__Theory,
	ast__Theory_Implementation,
	bool,
	i64,
	f64,
}

ast__Type :: ast__Expr
// }}}
// {{{ Statements
ast__Statement_Simple :: enum {
	Discard,
	Break,
	Continue,
}

ast__Statement_Return :: struct {
	expr: ^ast__Expr,
}

ast__Statement_For :: struct {
	init: ^ast__Statement,
	cond: ^ast__Statement,
	next: ^ast__Statement,
	body: ^ast__Statement,
}

ast__Statement_If :: struct {
	cond:     ^ast__Expr,
	if_true:  ^ast__Statement,
	if_false: ^ast__Statement,
}

ast__Statement_Block :: struct {
	steps: []ast__Statement,
}

ast__Statement_Declaration :: struct {
	name:  Name,
	value: ^ast__Expr,
	type:  ^ast__Type,
}

ast__Statement_Assignment :: struct {
	assign_to: ^ast__Expr,
	value:     ^ast__Expr,
}

ast__Statement :: union {
	ast__Statement_Simple,
	ast__Statement_Return,
	ast__Statement_For,
	ast__Statement_If,
	ast__Statement_Block,
	ast__Statement_Declaration,
	ast__Statement_Assignment,
}
// }}}
// {{{ Procedures
ast__Type_Proc :: struct {
	args:      []ast__Type,
	return_ty: ^ast__Type,
}

ast__Expr_Proc :: struct {
	names:          []Name,
	args:           []ast__Type,
	return_ty:      ^ast__Type,
	implementation: ast__Statement,
}
// }}}
// {{{ Theories
ast__Theory :: struct {
	imports:       []ast__Qualified_Name,
	names:         []ast__Qualified_Name,
	types:         []ast__Type,
	alias_names:   []ast__Qualified_Name,
	alias_targets: []ast__Qualified_Name,
}

ast__Theory_Implementation :: struct {
	inner: ast__Module,
}
// }}}
// {{{ External values
ast__External :: enum {
	Uniform,
	Attribute,
	Varying,
	UniformBuffer,
	Buffer, // SSBO
}
// }}}
// {{{ Declarations & modules
ast__Declaration :: struct {
	name:  ast__Qualified_Name,
	type:  ast__Type,
	value: ast__Expr,
}

ast__Module :: struct {
	imports:      []ast__Qualified_Name,
	declarations: []ast__Declaration,
}
// }}}
