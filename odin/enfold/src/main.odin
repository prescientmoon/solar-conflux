package enfold

import "core:log"

main :: proc() {
	context.logger = log.create_console_logger()
	source := #load("source.idea", string)

	lexer := mk_lexer(source)

	log.info("Hiii")

	for {
		tok := tokenize(&lexer)
		log.infof("Read token: %v", tok)
		if tok.kind == .Eof {
			break
		}
	}
}
