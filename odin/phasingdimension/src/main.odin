package phasingdimension

import "base:runtime"
import "core:log"

main :: proc() {
	context.logger = log.create_console_logger()

	state: State
	state.lps[0] = 8000
	state.lps[1] = 8000
	// add_card(&state, .Zeus, loc = .EMZ, fu = true)
	add_card(&state, .Necro, loc = .Hand)
	// add_card(&state, .Nibiru, loc = .Hand, opp = true)

	log.debug("Score: ", evaluate(&state))
}
