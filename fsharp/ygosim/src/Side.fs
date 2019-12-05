module Side

open Card

type Side =
    { field: Card option
      monsters: Card list
      spells: Card list
      graveyard: Card list
      deck: Card list }

let emptySide =
    { field = None
      monsters = []
      spells = []
      graveyard = []
      deck = [] }
