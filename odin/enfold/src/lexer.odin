package enfold

import "core:log"
import "core:odin/tokenizer"
import "core:unicode/utf8"

Source_Loc :: struct {
	index: uint,
	line:  uint,
	col:   uint,
}

Token_Kind :: enum {
	// Keywords
	Effect,
	Multi,
	List,
	Object,
	No_Align,

	// Identifier-like
	Identifier,
	Property,

	// Literals
	Bool,
	Integer,
	String,

	// Punctuation & operators
	Walrus,
	Equal,
	Comma,
	LParen,
	RParen,

	// Spacing
	Newline,
	Comment,
	Eof,
}

Token :: struct {
	from:    Source_Loc,
	kind:    Token_Kind,
	content: string,
	extra:   struct #raw_union {
		uint: i128,
	},
}

Lexer :: struct {
	source:     string,
	pos:        Source_Loc,
	curr:       rune,
	next_index: uint,
}

mk_lexer :: proc(source: string) -> Lexer {
	lexer := Lexer {
		source = source,
		pos = Source_Loc{line = 1, col = 1, index = 0},
		curr = 0,
		next_index = 0,
	}

	advance_rune(&lexer)

	return lexer
}

@(private = "file")
fail :: proc(lexer: ^Lexer, pos: Source_Loc, msg: string) {
	log.panicf("Error at %v: %v", pos, msg)
}

@(private = "file")
advance_rune :: proc(lexer: ^Lexer) {
	if lexer.next_index >= len(lexer.source) {
		lexer.pos.index = len(lexer.source)

		if lexer.curr == '\n' {
			lexer.pos.line += 1
			lexer.pos.col = 1
		}

		lexer.next_index = ~uint(0)
		lexer.curr = -1
		return
	}

	lexer.pos.index = lexer.next_index
	if lexer.curr == '\n' {
		lexer.pos.col = 1
		lexer.pos.line += 1
	} else {
		lexer.pos.col += 1
	}

	r, w := rune(lexer.source[lexer.next_index]), 1
	switch {
	case r == 0:
		fail(lexer, lexer.pos, "illegal character NUL")
	case r >= utf8.RUNE_SELF:
		r, w = utf8.decode_rune_in_string(lexer.source[lexer.next_index:])
		if r == utf8.RUNE_ERROR && w == 1 {
			fail(lexer, lexer.pos, "illegal UTF-8 encoding")
		} else if r == utf8.RUNE_BOM && lexer.next_index > 0 {
			fail(lexer, lexer.pos, "illegal byte order mark")
		}
	}

	lexer.next_index += uint(w)
	lexer.curr = r
}

tokenize :: proc(lexer: ^Lexer) -> (tok: Token) {
	ws: for {
		switch lexer.curr {
		case ' ', '\r', '\t':
			advance_rune(lexer)
		case:
			break ws
		}
	}

	tok.from = lexer.pos
	end_offset := 0

	switch ch := lexer.curr; true {
	// {{{ Identifiers & keywords
	case tokenizer.is_letter(ch):
		for tokenizer.is_letter(lexer.curr) || tokenizer.is_digit(lexer.curr) {
			advance_rune(lexer)
		}

		lit := string(lexer.source[tok.from.index:lexer.pos.index])

		tok.kind = .Identifier

		if lit == "do" {
			tok.kind = .Effect
		} else if lit == "multi" {
			tok.kind = .Multi
		} else if lit == "list" {
			tok.kind = .List
		} else if lit == "object" {
			tok.kind = .Object
		}
	// }}}
	// {{{ Integers
	case '0' <= ch && ch <= '9':
		res: i128

		for '0' <= lexer.curr && lexer.curr <= '9' {
			res = res * 10 + i128(lexer.curr - '0')
			advance_rune(lexer)
		}

		tok.kind = .Integer
		tok.extra.uint = res
	// }}}
	case:
		advance_rune(lexer)
		switch ch {
		// {{{ Punctuation & special characters
		case -1:
			tok.kind = .Eof
		case '\n':
			tok.kind = .Newline
		case '(':
			tok.kind = .LParen
		case ')':
			tok.kind = .RParen
		case ',':
			tok.kind = .Comma
		case '=':
			tok.kind = .Equal
		case ':':
			if lexer.curr == '=' {
				advance_rune(lexer)
				tok.kind = .Walrus
			} else {
				fail(lexer, tok.from, "expected = after :")
			}
		// }}}
		// {{{ Modifiers
		case '#':
			advance_rune(lexer)

			for tokenizer.is_letter(lexer.curr) || tokenizer.is_digit(lexer.curr) {
				advance_rune(lexer)
			}

			lit := string(lexer.source[tok.from.index:lexer.pos.index])

			if lit == "#noalign" {
				tok.kind = .No_Align
			} else {
				fail(lexer, tok.from, "unknown modifier")
			}
		// }}}
		// {{{ Strings
		case '"':
			advance_rune(lexer)

			// TODO: escaping and whatnot
			for lexer.curr != '"' {
				advance_rune(lexer)
			}

			advance_rune(lexer)

			tok.kind = .String
		// }}}
		// {{{ Dot access
		case '.':
			advance_rune(lexer)

			for tokenizer.is_letter(lexer.curr) || tokenizer.is_digit(lexer.curr) {
				advance_rune(lexer)
			}

			tok.kind = .Property
		// }}}
		// {{{ Comments
		case '-':
			if lexer.curr == '-' {
				advance_rune(lexer)

				for lexer.curr != '\n' && lexer.curr >= 0 {
					advance_rune(lexer)
				}

				// Strip CR from line comments
				for lexer.source[int(lexer.pos.index) + end_offset - 1] == '\r' {
					end_offset -= 1
				}

				tok.kind = .Comment
			} else {
				fail(lexer, tok.from, "expected - after -")
			}
		// }}}
		case:
			fail(lexer, tok.from, "unexpected character")
		}
	}

	tok.content = lexer.source[tok.from.index:int(lexer.pos.index) + end_offset]

	return tok
}
