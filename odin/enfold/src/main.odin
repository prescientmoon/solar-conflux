package enfold

import "core:log"
import "core:mem/virtual"

Enfold_Error :: union {
	Lexer_Error,
	Parser_Error,
	Parser_Cancellation,
}

parser_cancelled :: proc(err: Enfold_Error) -> bool {
	_, ok := err.(Parser_Cancellation)
	return ok
}

main :: proc() {
	context.logger = log.create_console_logger()

	arena: virtual.Arena
	if err := virtual.arena_init_growing(&arena); err != .None {
		log.panicf("Failed to init parsing arena: %v", err)
	}

	defer virtual.arena_destroy(&arena)

	source := #load("source.idea", string)
	parser, err := mk_parser(source, virtual.arena_allocator(&arena))
	if err != nil {log.error(err)}

	expr: Expr
	expr, err = parse_toplevel_expr(&parser)

	if err != nil {log.error(err)} else {
		log.infof("Expr: %#v", expr)
	}

	log.infof(
		"Parsing finished, using %v/%v",
		Bytes(arena.total_used),
		Bytes(arena.total_reserved),
	)
}
