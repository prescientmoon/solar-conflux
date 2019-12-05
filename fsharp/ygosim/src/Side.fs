module Side

open Card

type Side =
    { field: Card
      monsters: Card list
      spells: Card list
      graveyard: Card list
      deck: Card list }
