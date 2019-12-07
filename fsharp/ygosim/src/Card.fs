module Card

// TODO: actually implement it
type BaseCard =
    { name: string
      text: string }

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


type Card =
    | Monster of BaseCard * MonsterCardDetails
    | Spell of BaseCard * SpellCardDetails
    | Trap of BaseCard * TrapCardDetails

// TODO: actually make this do what its supposed to
type CardInstance = Card
