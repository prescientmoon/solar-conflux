module Board

open Player

type Board =
    { players: Player * Player }

let emptyBoard = { players = (initialPlayer 8000, initialPlayer 8000) }
