module Card

open FSharpPlus.Lens

module Effect =
    type Condition<'s> = 's -> bool

    type Action<'s> =
        { condition: Condition<'s>
          resolution: 's -> 's }

    module Action =
        let inline condition f action = f action.condition <&> fun c -> { action with condition = c }
        let inline resolution f action = f action.resolution <&> fun r -> { action with resolution = r }

    type EffectType =
        | Trigger
        | Ignition
        | Maintanence

    type Effect<'s> =
        { cost: Action<'s>
          resolve: Action<'s>
          _type: EffectType }

    module Effect =
        let inline cost f effect = f effect.cost <&> fun c -> { effect with cost = c }
        let inline resolve f effect = f effect.resolve <&> fun r -> { effect with resolve = r }
        let inline _type f effect = f effect._type <&> fun t -> { effect with _type = t }


module Card =
    open Effect

    type BaseCard<'s> =
        { name: string
          text: string
          effects: Effect<'s> list }

    module BaseCard =
        let inline name f card = f card.name <&> fun v -> { card with name = v }
        let inline text f card = f card.text <&> fun v -> { card with text = v }
        let inline effects f card = f card.effects <&> fun v -> { card with effects = v }

    type SpellCardType =
        | NormalSpell
        | Field
        | Equip
        | ContinuosSpell
        | QuickPlay
        | Ritual

    type TrapCardType =
        | NormalTrap
        | Counter
        | ContinuosTrap

    type Attribute =
        | Dark
        | Light
        | Water
        | Fire
        | Earth
        | Wind
        | Divine

    type Race =
        | Aqua
        | Beast
        | BeastWarrior
        | Creator
        | Cyberse
        | Dinosaur
        | DivineBeast
        | Dragon
        | Fairy
        | Fiend
        | Fish
        | Insect
        | Machine
        | Plant
        | Psychic
        | Pyro
        | Reptile
        | Rock
        | SeaSerpent
        | Spellcaster
        | Thunder
        | Warrior
        | WingedBeast
        | Wyrm
        | Zombie

    type SpellCardDetails =
        { spellType: SpellCardType }

    module SpellCardDetails =
        let inline spellType f card = f card.spellType <&> fun v -> { card with spellType = v }

    type TrapCardDetails =
        { trapType: TrapCardType }

    module TrapCardDetails =
        let inline trapType f card = f card.trapType <&> fun v -> { card with trapType = v }


    type MonsterCardDetails =
        { attack: int
          defense: int
          attribute: Attribute
          level: int }

    module MonsterCardDetails =
        let inline attack f card = f card.attack <&> fun v -> { card with attack = v }
        let inline trapType f card = f card.defense <&> fun v -> { card with defense = v }
        let inline attribute f card = f card.attribute <&> fun v -> { card with attribute = v }
        let inline level f card = f card.level <&> fun v -> { card with level = v }


    type Card<'s> =
        | Monster of BaseCard<'s> * MonsterCardDetails
        | Spell of BaseCard<'s> * SpellCardDetails
        | Trap of BaseCard<'s> * TrapCardDetails

    module Card =
        let inline baseCard f card = _1 f card
        let inline cardDetails f card = _2 f card

    type CardInstance<'s> = Card<'s>

module Decklist =
    type Decklist =
        { main: int list
          side: int list
          extra: int list }
