package enfold

import "core:fmt"
import "core:log"
import "core:mem/virtual"
import "core:os"
import "core:strings"

Enfold_Error :: union {
	Lexer_Error,
	Parser_Error,
	Parser_Cancellation,
	Evaluator_Error,
}

parser_cancelled :: proc(err: Enfold_Error) -> bool {
	_, ok := err.(Parser_Cancellation)
	return ok
}

main_impl :: proc() -> int {
	context.logger = log.create_console_logger()

	arena: virtual.Arena
	if err := virtual.arena_init_growing(&arena); err != .None {
		log.panicf("Failed to init parsing arena: %v", err)
	}

	defer virtual.arena_destroy(&arena)
	defer log.infof(
		"Parsing finished, using %v/%v",
		Bytes(arena.total_used),
		Bytes(arena.total_reserved),
	)

	source := #load("../experiments/source2.idea", string)
	parser, err := mk_parser(source, virtual.arena_allocator(&arena))
	if err != nil {log.error(err)}

	expr: Expr
	expr, err = parse_toplevel_block(&parser)

	if err != nil {
		log.error(err)
		return 1
	} else {
		log.infof("Expr: %#v", expr)
	}

	evaluator := mk_evaluator(parser.alloc)
	neffect: NEffect
	neffect, err = expr_to_normalized_effect(&evaluator, expr)

	if err != nil {
		log.error(err)
		return 1
	} else {
		log.infof("Effect: %#v", neffect)
	}

	cg := mk_lua_codegen(parser.alloc)
	effect_to_lua(&cg, neffect)

	f, _ := os.open(
		"out.lua",
		os.O_WRONLY | os.O_TRUNC | os.O_CREATE,
		mode = os.S_IRUSR | os.S_IWUSR | os.S_IRGRP | os.S_IROTH,
	)
	defer os.close(f)

	lua_code := strings.to_string(cg.out)
	// log.info(lua_code)
	fmt.fprint(f, lua_code)

	return 0
}

main :: proc() {
	exit_code := main_impl()
	if exit_code != 0 {
		os.exit(exit_code)
	}
}
