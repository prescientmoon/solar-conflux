module Player

open Side

type Player =
    { lifePoints: int
      side: Side }

let inflictDamage (player: Player) amount = { player with lifePoints = player.lifePoints - amount }

let initialPlayer lp =
    { lifePoints = lp
      side = emptySide }
