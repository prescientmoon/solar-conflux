module Card


module Effect =
    type Condition<'s> = 's -> bool

    type Action<'s> =
        { condition: Condition<'s>
          resolution: 's -> 's }

    type EffectType =
        | Trigger
        | Ignition

    type Effect<'s> =
        { cost: Action<'s>
          resolve: Action<'s>
          _type: EffectType }


module Card =
    open Effect

    type BaseCard<'s> =
        { name: string
          text: string
          effects: Effect<'s> list }

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

    type TrapCardDetails =
        { trapType: TrapCardType }


    type MonsterCardDetails =
        { attack: int
          defense: int
          attribute: Attribute
          level: int }


    type Card<'s> =
        | Monster of BaseCard<'s> * MonsterCardDetails
        | Spell of BaseCard<'s> * SpellCardDetails
        | Trap of BaseCard<'s> * TrapCardDetails

    // TODO: actually make this do what its supposed to
    type CardInstance<'s> = Card<'s>

module Decklist =
    type Decklist =
        { main: int list
          side: int list
          extra: int list }
