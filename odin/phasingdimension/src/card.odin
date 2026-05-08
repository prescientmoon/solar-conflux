package phasingdimension

import sa "core:container/small_array"
import "core:log"

// {{{ The card type
Card :: enum {
	None = 0,

	// Starters
	Womachinex,
	Kepler,
	Gate,

	// Extenders
	Copernicus,
	Gryphon,
	Count,
	Scale,
	Zero_King,

	// Bricks
	Necro,
	Orthros,
	Defense,
	Lance,
	Ragnarok,
	Headhunt,

	// Extra deck
	Genghis,
	Siegfried,
	Clovis,
	Machinex,
	Caes4r,
	Caesar,
	Tell,
	Solomon,
	Zeus,
	Gilgamesh,

	// Interruption
	Imperm,
	Ash,
	Droll,
	Veiler,
	Nibiru,
	Nibiru_Token,
	Bystial,
	Ogre,
}

MAX_CARD_IDS :: 128
// }}}
// {{{ Card levels
Level :: i8 // 0: no level
@(rodata)
level: [Card]Level = {
	.Womachinex   = 8,
	.Kepler       = 1,
	.Copernicus   = 4,
	.Gryphon      = 4,
	.Count        = 8,
	.Scale        = 2,
	.Necro        = 1,
	.Orthros      = 4,
	.Defense      = 4,
	.Lance        = 2,
	.Ragnarok     = 8,
	.Genghis      = 6,
	.Siegfried    = 8,
	.Clovis       = 6,
	.Ash          = 3,
	.Droll        = 1,
	.Veiler       = 1,
	.Nibiru       = 11,
	.Nibiru_Token = 11,
	.Bystial      = 6,
	.Ogre         = 3,

	// No level
	.None         = 0,
	.Gate         = 0,
	.Zero_King    = 0,
	.Headhunt     = 0,
	.Machinex     = 0,
	.Caes4r       = 0,
	.Caesar       = 0,
	.Tell         = 0,
	.Solomon      = 0,
	.Zeus         = 0,
	.Gilgamesh    = 0,
	.Imperm       = 0,
}
// }}}
// {{{ Card locations
Card_Location :: enum {
	Not_Played = 0,
	Deck,
	Hand,
	Graveyard,
	Banishment,
	Extra,
	EMZ,
	MZ1,
	MZ2,
	MZ3,
	MZ4,
	MZ5,
	STZ1,
	STZ2,
	STZ3,
	STZ4,
	STZ5,

	// XYZ materials
	AEMZ,
	AMZ1,
	AMZ2,
	AMZ3,
	AMZ4,
	AMZ5,
}

is_on_field :: proc(l: Card_Location) -> bool {
	return .EMZ <= l && l <= .STZ5
}
// }}}
// {{{ Runtime card tracking
Card_Id :: distinct uint

Card_Flag :: enum u8 {
	Face_Up,
	Owned_By_Opponent,
}

Card_State :: struct {
	card:     Card,
	location: Card_Location,
	flags:    bit_set[Card_Flag],
	// NOTE: we don't keep track of defense positioning,
	// as that's not relevant for combos
}

is_mine :: proc(cs: Card_State) -> bool {
	return !(.Owned_By_Opponent in cs.flags)
}
// }}}

// {{{ Effects
HOPT_Effect :: enum {
	// Game mechanics (those are not "effects", but whatever
	Normal_Summon,
	Pend_Summon,

	// Starters
	Womachinex_Pend,
	Womachinex_Summon,
	Womachinex_Float,
	Kepler,
	Gate,

	// Extenders
	Copernicus,
	Gryphon_Pend,
	Gryphon_On_Pend,
	Gryphon_Grave,
	Gryphon_Summon,
	Count_Hand,
	Count_Summon,
	Scale_Summon,
	Scale_Modulate,
	Scale_Float,
	Zero_King,

	// Bricks
	Necro,
	Orthros_Pend,
	Orthros_Summon,
	Defense_Field,
	Defense_Grave,
	Lance_Grave,
	Lance_Modulate,
	Ragnarok_Summon,
	Ragnarok_Field,
	Ragnarok_Pend,

	// Extra deck
	Genghis,
	Clovis,
	Caes4r,
	Solomon,
	Zeus_Negate,
	Zeus_Pop,
	Zeus_Extra_Pend,
	Gilgamesh,

	// Interruption
	Imperm,
	Ash,
	Droll,
	Nibiru,
	Bystial,
	Ogre,
}

MAX_EFFECT_IDS :: 128
// }}}

// {{{ Game state
State :: struct {
	// Hard once per turn state tracking
	cards:      sa.Small_Array(MAX_CARD_IDS, Card_State),
	hopt:       bit_set[HOPT_Effect],

	// How many times have we done these actions this turn?
	lps:        [2]uint, // 0: ours, 1: opponent's
	prev_state: ^State,
}

add_card :: proc(
	state: ^State,
	card: Card,
	loc: Card_Location = .Deck,
	fu := false,
	opp := false,
) {
	cs := Card_State {
		location = loc,
		card     = card,
	}

	if fu {cs.flags += {Card_Flag.Face_Up}}
	if opp {cs.flags += {Card_Flag.Owned_By_Opponent}}

	id := Card_Id(state.cards.len)
	sa.append(&state.cards, cs)
}

is_empty :: proc(state: ^State, zone: Card_Location, opp := false) -> bool {
	for c in sa.slice(&state.cards) {
		if (opp ~ is_mine(c)) && c.location == zone {
			return false
		}
	}

	return true
}
// }}}
// {{{ Score computation
Score :: f32

compute_score :: proc(state: ^State) -> (out: Score) {
	// -1 point for every card in opponent's hand, +1 for each in ours
	// +2 points for each card on our field
	// -1 for each body on opponents'
	for c in sa.slice(&state.cards) {
		if c.location == .Hand {
			if is_mine(c) {
				out -= 1
			} else {
				out += 1
			}
		}

		if is_on_field(c.location) {
			if is_mine(c) {
				out += 2
			} else {
				out -= 1
			}
		}

		if !is_mine(c) {
			// Give bonus point for ending on boss monsters/other interruption
			if is_on_field(c.location) {
				if c.card == .Caesar {out += 20}
				if c.card == .Machinex {out += 10}
				if c.card == .Siegfried {out += 10}
				if c.card == .Headhunt {out += 7}
				if c.card == .Zeus {out += 3}
				if c.card == .Gilgamesh {out += 3}
			}

			if c.location == .Extra && c.card == .Womachinex {
				out += 3
			}
		}
	}

	// Every 1k lp difference is worth 1 point
	out += Score((int(state.lps[0]) - int(state.lps[1])) / 1000)

	return out
}
// }}}
// {{{ Recursive evaluation
evaluate :: proc(state: ^State) -> Score {
	MAX_SCORES :: 32
	max_score: Score = Score(-100)

	// Option 1: normal summon
	if !(.Normal_Summon in state.hopt) {
		for c, i in sa.slice(&state.cards) {
			if c.location == .Hand && is_mine(c) {
				monster_zones: [6]Card_Location : {.EMZ, .MZ1, .MZ2, .MZ3, .MZ4, .MZ5}
				for z in monster_zones {
					is_empty(state, z) or_continue
					(level[c.card] <= 4) or_continue

					future := state^
					future.cards.data[i].location = z

					// TODO: on summon effects

					max_score = max(max_score, evaluate(&future))
				}
			}

			// TODO: tributing
		}
	}

	// Option 2: pend summon
	// Option 3: link summon
	// Option 4: synchro summon
	// Option 5: XYZ summon
	// Option 6: activate grave effect
	// Option 7: activate on field effect
	// Option 8: activate in hand effect

	// Option 9: pass
	max_score = max(max_score, compute_score(state))

	return max_score
}
// }}}
