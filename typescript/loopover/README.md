# Loopover

## Installation

```sh
npm install @adrielus/loopover
```

## Usage

### Creating a game

To create a game use the `createGame` helper and pass it the `width` and the `height` of the board:

```ts
import { createGame } from '@adrielus/loopover'

// this creates a 3x3 board
const game = createGame(3, 3)
```

The `createGame` method returns a `GameState` instance.

### Getting the metadata from a state

Each `GameState` instance has a `width` and a `height` prop:

```ts
game.width // 3
game.height // 3
```

### Iterating over a state

The `GameState` has a `[Symbol.iterator]` prop, so you can iterate over it with a for of loop:

```ts
for (const piece of game) {
    // piece is just an int
}
```

You can also transform it into an array and then use a normal for loop:

```ts
const array = [...game]

for (let index = 0; index < array.length; index++) {
    const piece = array[index]
}
```

Or you can use the cells getter which will return an array:

```ts
for (let index = 0; index < game.cells.length; i++) {
    const piece = array[index]
}
```

### Applying moves:

The GameState instance has 2 methods for applying moves: `moveX` and `moveY`. Both method accept a direction (1 or -1) and a layer.

Example:

If the board is in the following position:

|     |     |     |
| --- | --- | --- |
| 1   | 2   | 3   |
| 4   | 5   | 6   |
| 7   | 8   | 9   |

and you apply:

```ts
game.moveX(-1, 1)
```

The board will look like:

|     |     |     |
| --- | --- | --- |
| 1   | 2   | 3   |
| 5   | 6   | 4   |
| 7   | 8   | 9   |

> Note: both `moveX` and `moveY` **_WON'T_** mutate the original game state, but will return the new state.

# Contributing

First, clone this repo:

```sh
git clone https://github.com/Mateiadrielrafael/loopover
cd loopover
```

Then use **_pnpm_** to install the dependencies:

```sh
pnpm install
```

You can use the `build` command to build the package (this is dont automatically by github actions):

```sh
pnpm run build
```
