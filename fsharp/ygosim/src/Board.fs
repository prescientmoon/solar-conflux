module Board

module Side =
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


module Player =
    open Side

    type Player =
        { lifePoints: int
          side: Side }

    let inflictDamage (player: Player) amount = { player with lifePoints = player.lifePoints - amount }

    let initialPlayer lp =
        { lifePoints = lp
          side = emptySide }



module Board =
    open Player

    type Board =
        { players: Player * Player }

    let emptyBoard = { players = (initialPlayer 8000, initialPlayer 8000) }
