module Card

open FSharpPlus.Lens
open FSharpPlus.Operators

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

module BaseCard =
    open Effect

    type BaseCard<'s> =
        { name: string
          text: string
          effects: Effect<'s> list }

    module BaseCard =
        let inline name f card = f card.name <&> fun v -> { card with name = v }
        let inline text f card = f card.text <&> fun v -> { card with text = v }
        let inline effects f card = f card.effects <&> fun v -> { card with effects = v }


module MonsterTypes =
    open BaseCard


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

    type MonsterCardDetails =
        { attack: int
          defense: int
          level: int
          attribute: Attribute
          race: Race }

    module MonsterCardDetails =
        let inline attack f card = f card.attack <&> fun v -> { card with attack = v }
        let inline trapType f card = f card.defense <&> fun v -> { card with defense = v }
        let inline attribute f card = f card.attribute <&> fun v -> { card with attribute = v }
        let inline level f card = f card.level <&> fun v -> { card with level = v }


    type Monster<'s> = BaseCard<'s> * MonsterCardDetails

module Card =
    open BaseCard
    open MonsterTypes

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

    type SpellCardDetails =
        { spellType: SpellCardType }

    module SpellCardDetails =
        let inline spellType f card = f card.spellType <&> fun v -> { card with spellType = v }

    type TrapCardDetails =
        { trapType: TrapCardType }

    module TrapCardDetails =
        let inline trapType f card = f card.trapType <&> fun v -> { card with trapType = v }


    type Card<'s> =
        | Monster of Monster<'s>
        | Spell of BaseCard<'s> * SpellCardDetails
        | Trap of BaseCard<'s> * TrapCardDetails

    module Card =
        let inline baseCard f card = _1 f card
        let inline cardDetails f card = _2 f card

        let inline level f card = (_2 << MonsterCardDetails.level) f card


module CardInstance =
    open Card

    type CardInstance<'s> =
        { template: Card<'s>
          id: int }

    module CardInstance =
        let inline template f card = f card.template <&> fun v -> { card with template = v }
        let inline _id f card = f card.id <&> fun v -> { card with id = v }


module Monster =
    open MonsterTypes
    open CardInstance
    open Card

    let monster card: option<Monster<'a> * int> =
        match card.template with
        | Monster m -> Some(m, card.id)
        | _ -> None


    let toCardInstance (card: Monster<'a>, _id): CardInstance<'a> =
        { template = Monster card
          id = _id }

module MonsterInstance =
    open MonsterTypes

    type MonsterInstance<'s> = Monster<'s> * int

    module private Internals =
        // idk how to preperly override (=)
        let areEqual (_, _id1) (_, _id2) = _id1 = _id2
        let (==) = areEqual

    open Internals

    let withoutInstance instance = areEqual instance >> not

module Decklist =
    type Decklist =
        { main: int list
          side: int list
          extra: int list }
