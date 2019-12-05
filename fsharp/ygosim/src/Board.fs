module Board

open Player

type Board =
    { players: Player.Player * Player }
