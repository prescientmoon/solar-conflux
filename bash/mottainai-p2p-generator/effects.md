### Paper

- Poem:
  Affect you complete a work, you may return it. Complete the top card of the deck for free.

  > Can this work on itself? YES

  ```
  Trigger:
    event.kind == work-completed &&
    event.work in works[you]
  Effect:
    return(event.work)
    complete(deck.top)
  ```

- Pinwheel:
  At night, you may return a card from your hand. If you do, you may draw a card.

  ```
  Trigger:
    event.kind == at-night &&
    len(hands[you]) > 0
  Effect:
    return(choose(hand))
    draw(1)
  ```

- Scroll:
  +3 points

  ```
  Mandatory
  Trigger: compute-scores
  Effect:
    score[you] += 3
  ```

- Curtain:
  Opponents skip your TAILOR or SMITH tasks unless they reveal a matching card from their hand

  > Does this apply to prayers? YES (they can't convert if the action gets skipped
  > Does this apply once, or for each iteration? ONCE (it applies to the task, not the action)

  ```
  Mandatory
  Trigger:
    event.kind == pre-task &&
    event.player == opponent &&
    event.owner == you &&
    event.action.kind in { TAILOR, SMITH }
  Effect:
    card := choose(event.player, hand, event.action.kind, opt=true)
    if card == null:
      skip(event)
  ```

- Crane:
  You may return PAPER from your craft bench to count as one support for any work.

  ```
  Trigger: compute-supports
  Effect:
    card := choose(craft_bench)
    return(card)
    event.supports.add(card)
  ```

- Fan:
  Before a TAILOR action, you may reveal the top three cards of the deck. If you do, return two of them and put the third back on top of it.

  > Does this only apply to my actions?
  > Does this apply to prayers? NO

  ```
  Trigger:
    event.kind == pre-action &&
    event.action.kind in { TAILOR } &&
    len(deck) >= 3
  Effect:
    choices := deck[0], deck[1], deck[2]
    choice := choose(reveal(choices))

    for card in choices:
      if choice != card:
        return(card)
  ```

- Lampshade
  You win CLAY and METAL sales ties.

  ```
  Mandatory
  Trigger:
    event.kind == sales-tie &&
    event.material in { CLAY, METAL }
  Effect:
    event.players[you].winner = true
  ```

- Plane
  After a POTTER action, if you collected a material, you may move one of your works from one wing to the other.

  ```
  Trigger:
    event.kind == post-action &&
    event.action.kind == POTTER &&
    event.collected
  Effect:
    work := choose(works)
    work.wing = !work.wing
  ```

- Straw
  CLOTH and CLAY works each require one fewer support to complete with a SMITH action.

  ```
  Mandatory
  Trigger:
    event.kind == compute-supports &&
    event.method == SMITH &&
    event.work.kind in {CLOTH, CLAY} &&
    event.support_count > 0
  Effect:
    event.support_count -= 1
  ```

- Deck of cards:
  After a SMITH action, if you completed a PAPER work, you may draw a card fo your waiting area.

  ```
  Trigger:
    event.kind == work-completed &&
    event.card.kind == PAPER &&
    event.method == SMITH
  Effect:
    draw(1)
  ```

- Sketch:
  In the morning, you may move one of your helpers to become your new task.

  ```
  Trigger:
    event.kind == in-the-morning &&
    task == null &&
    len(helpers) > 0
  Effect:
    move(choose(helpers), task)
  ```

- Doll:
  In the morning, you may move an opponent’s task to become your new task. It gives you one extra action.

  ```
  Trigger:
    event.kind == in-the-morning &&
    task == null &&
    len(tasks) > 0
  Effect:
    move(choose(tasks), task)
    task.turn_bonus_actions += 1
  ```

### Stone

- Statue
  After you complete this, transfer two materials from the floor to your craft bench.

  ```
  Mandatory
  Trigger:
    event.kind == work-completed &&
    event.card == self
  Effect:
    cards := choose(floor, 2)
    for card in cards:
      if card != null:
        move(card, craft_bench)
  ```

- Pillar
  All sales of your most sold resource type are considered covered

  ```
  Mandatory
  Trigger:
    event.kind == pre-compute-sales &&
    len(sales) > 0
  Effect:
    max_mats := find_maximums(materials, λm. len(m))
    mat := choose(max_maths)
    for sale in sales:
      if sale.material != mat:
        event.sales[sale].covered = true
  ```

- Frog
  After you complete this, if no opponent has fewer works than you, take an extra turn after this one.

  > What happens if you do this twice? I think you just take extra turns

  ```
  Mandatory
  Trigger:
    event.kind == work-completed &&
    event.card == self &&
    fewest-works
  Effect:
    game.next_turn = you
    # Might need changing if we need to support multiple ones
  ```

- Tablet
  After you complete this, either return all cards on the floor, or restock the floor from the deck until it has both a STONE and a METAL.

  ```
  Mandatory
  Trigger:
    event.kind == work-completed &&
    event.card == self
  Effect:
    if choose(e1, e2) == e1:
      for card in floor:
        return(card)
    else:
      while !subset({STONE, METAL}, floor) && len(deck) > 0:
        move(deck.top, floor)
  ```

- Stool
  After you complete a STONE, CLAY, or METAL work, you may draw a card to your waiting area.

  ```
  Trigger:
    event.kind == work-completed &&
    event.card.kind in {STONE, CLAY, METAL}
  Effect:
    draw(1)
  ```

- Go set
  All your STONE works count as being in both wings at the same time.

  ```
  Mandatory
  Trigger:
    event.kind == is-in-wing &&
    event.card.kind == STONE
  EFFECT:
    event.result = true
  ```

- Fountain
  Before a CLERK task, you may reveal MONK cards from hand. Each one counts as a CLERK helper for the task.

  > Can these be covered?

  ```
  Trigger:
    event.kind == compute-helpers &&
    event.action.kind == CLERK
  EFFECT:
    cards := choose(subsets(hand, MONK))

    # Assuming these can't be covered
    event.extra_helpers += len(cards)
  ```

- Tower:
  Opponents cannot use your CLERK, MONK, or POTTER tasks unless they reveal a matching card from their hand.

  > Does this apply to prayers? YES (they can't convert it to a player if it gets skipped)

  ```
  Mandatory
  Trigger:
    event.kind == pre-task &&
    event.player == opponent &&
    event.owner == you &&
    event.action.kind in { CLERK, MONK, POTTER }
  Effect:
    card := choose(event.player, hand, event.action.kind, opt=true)
    if card == null:
      skip(event)
  ```

- Daitoro:
  In the morning, you may restock the floor from the top of the deck until there are three cards on the floor.

  ```
  Trigger:
    event.kind == in-the-morning &&
    len(floor) < 3
  Effect:
    while len(floor) < 3:
      move(deck.top, floor)
  ```

- Amulet:
  After you complete a work, you may sell a material from your craft bench.

  ```
  Trigger:
    event.kind == work-completed &&
    len(craft_bench) > 0
  Effect:
    card := choose(craft_bench)
    move(card, sales)
  ```

- Bench:
  +2 points for each of your STONE works.
  ```
  Mandatory
  Trigger: compute-scores
  Effect:
    for card in works:
      if card.kind == STONE:
        scores[you] += 2
  ```

### Cloth

- Kite:
  In the morning, you may transfer a card from your hand to any craft bench. If you do, treat Kite as an exact copy of one of that player’s works until the end of your turn.

  > This says "treat Kite". Does this means it treats _all_ copies of Kite?
  > Is this once per turn? Nope, you can copy Kite and keep going

  ```
  Trigger:
    event.kind == in-the-morning &&
    len(hand) > 0 &&
    len(works[everyone]) > 0
  Effect:
    card := choose(works[everyone])
    move(choose(hand), workplace[card.owner])
    game.turn_remaps[kite] = card
  ```

- Umbrella:
  In the morning, you may add a card to the floor from the top of the deck. If you do, you may convert a matching helper into a sale.

  > Is this once per turn? I think so

  ```
  Trigger: in-the-morning
  Effect:
    move(deck.top, floor)
    card := choose(helpers, opt=true)
    if card != null:
      move(card, sales)
  ```

- Socks:
  For a POTTER action, you may collect the top card of the deck instead of a card from the floor.

  ```
  Trigger:
    event.kind == pre-action &&
    event.action.kind == POTTER &&
    event.source == null # No point in activating two copies of this
  Effect:
    event.source = deck.top
  ```

- Quilt:
  You win PAPER, STONE, and CLOTH sales ties. All sales of these material types are considered covered.

  ```
  Mandatory
  Trigger:
    event.kind == sales-tie &&
    event.material in { PAPER, STONE, CLOTH }
  Effect:
    event.players[you].winner = true

  Mandatory
  Trigger:
    event.kind == pre-compute-sales
  Effect:
    for sale in sales:
      if sale.material in { PAPER, STONe, CLOTH }:
        event.sales[sale].covered = true
  ```
