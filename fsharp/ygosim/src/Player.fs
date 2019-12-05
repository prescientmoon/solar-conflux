module Player

open Side

type Player =
    { lifePoints: int
      side: Side }

let inflictDamage (player: Player) amount = { player with lifePoints = player.lifePoints - amount }


type PlayerControllerAction =
    | ChooseZone
    | BoardUpdate

type PlayerActionPayload = PlayerControllerAction * Side

type PlayerController = PlayerController of (PlayerControllerAction -> PlayerController)
 