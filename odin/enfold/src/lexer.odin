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
	Global,

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
}

Lexer :: struct {
	source:     string,
	pos:        Source_Loc,
	curr:       rune,
	next_index: uint,
}

Lexer_Error :: struct {
	pos: Source_Loc,
	msg: string,
}

mk_lexer :: proc(source: string) -> (lexer: Lexer, err: Enfold_Error) {
	lexer = Lexer {
		source = source,
		pos = Source_Loc{line = 1, col = 0, index = 0},
		curr = 0,
		next_index = 0,
	}

	advance_rune(&lexer) or_return

	return lexer, nil
}

@(require_results)
@(private = "file")
advance_rune :: proc(lexer: ^Lexer) -> (err: Enfold_Error) {
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
		return Lexer_Error{lexer.pos, "illegal character NUL"}
	case r >= utf8.RUNE_SELF:
		r, w = utf8.decode_rune_in_string(lexer.source[lexer.next_index:])
		if r == utf8.RUNE_ERROR && w == 1 {
			return Lexer_Error{lexer.pos, "illegal UTF-8 encoding"}
		} else if r == utf8.RUNE_BOM && lexer.next_index > 0 {
			return Lexer_Error{lexer.pos, "illegal byte order mark"}
		}
	}

	lexer.next_index += uint(w)
	lexer.curr = r

	return
}

tokenize :: proc(lexer: ^Lexer) -> (tok: Token, err: Enfold_Error) {
	ws: for {
		switch lexer.curr {
		case ' ', '\r', '\t':
			advance_rune(lexer) or_return
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
			advance_rune(lexer) or_return
		}

		lit := string(lexer.source[tok.from.index:lexer.pos.index])

		tok.kind = .Identifier

		if lit == "do" || lit == "effect" {
			tok.kind = .Effect
		} else if lit == "multi" {
			tok.kind = .Multi
		} else if lit == "list" {
			tok.kind = .List
		} else if lit == "object" {
			tok.kind = .Object
		} else if lit == "global" || lit == "globals" {
			tok.kind = .Global
		} else if lit == "false" || lit == "true" {
			tok.kind = .Bool
		}

	// }}}
	// {{{ Integers
	case '0' <= ch && ch <= '9':
		for '0' <= lexer.curr && lexer.curr <= '9' {
			advance_rune(lexer) or_return
		}

		tok.kind = .Integer
	// }}}
	case:
		advance_rune(lexer) or_return
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
				advance_rune(lexer) or_return
				tok.kind = .Walrus
			} else {
				return tok, Lexer_Error{tok.from, "expected = after :"}
			}
		// }}}
		// {{{ Modifiers
		case '#':
			for tokenizer.is_letter(lexer.curr) || tokenizer.is_digit(lexer.curr) {
				advance_rune(lexer) or_return
			}

			lit := string(lexer.source[tok.from.index:lexer.pos.index])

			if lit == "#noalign" {
				tok.kind = .No_Align
			} else {
				return tok, Lexer_Error{tok.from, "unknown modifier"}
			}
		// }}}
		// {{{ Strings
		case '"':
			escaped := false
			for {
				ch := lexer.curr
				advance_rune(lexer) or_return

				if escaped {
					escaped = false
				} else if ch == '\\' {
					escaped = true
					continue
				} else if ch == '"' {
					break
				}
			}

			tok.kind = .String
		// }}}
		// {{{ Dot access
		case '.':
			c := 0
			for tokenizer.is_letter(lexer.curr) || tokenizer.is_digit(lexer.curr) {
				advance_rune(lexer) or_return
				c += 1
			}

			if c == 0 {
				return tok, Lexer_Error{tok.from, "expected modifier name"}
			}

			tok.kind = .Property
		// }}}
		// {{{ Comments
		case '-':
			if lexer.curr == '-' {
				advance_rune(lexer) or_return

				for lexer.curr != '\n' && lexer.curr >= 0 {
					advance_rune(lexer) or_return
				}

				// Strip CR from line comments
				for lexer.source[int(lexer.pos.index) + end_offset - 1] == '\r' {
					end_offset -= 1
				}

				tok.kind = .Comment
			} else {
				return tok, Lexer_Error{tok.from, "expected - after -"}
			}
		// }}}
		case:
			return tok, Lexer_Error{tok.from, "unexpected character"}
		}
	}

	tok.content = lexer.source[tok.from.index:int(lexer.pos.index) + end_offset]

	return tok, nil
}
