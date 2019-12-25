![npm (scoped)](https://img.shields.io/npm/v/@adrielus/option?style=for-the-badge)
![npm bundle size (scoped)](https://img.shields.io/bundlephobia/minzip/@adrielus/option?style=for-the-badge)
[![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com)

# Option

Probably the most opinionated implementation of the Option type for typescript.

## Features:

-   Large amount of helpers (curently 25), more than f#'s and elm's core libraries combined.
-   Typesafe:
    ```ts
    const foo0: Option<string> = None // works
    const foo1: Option<string> = Some('foo1') // works
    const foo2: Option<string> = 'foo2' // errors out
    const foo3: Option<string> = null // errors out
    const foo4: Option<string> = Some(4) // errors out
    ```
-   Native equality:
    ```ts
    Some(7) === Some(7) // true
    Some(7) === Some(5) // false
    Some(7) === None // false
    ```

## Limitations

Both limitaions bellow come from the lack of nominal-typing offered by typescript and are inherited from the `Brand` type offered by the [utility-types](https://github.com/piotrwitek/utility-types) library

-   Due to the way the library works (using the `Brand`
    type from [utility-types](https://github.com/piotrwitezutility-types)) `Some(4) === 4` will return true, similarly to how `4 == "4"` returns true (except in this libraries case the `===` operator will behave the same way).
-   The inner value of `Option` cannot have a `__brand` prop
    (well, tehnically it can but it would be overwritten by the `Brand` type from [utility-types](https://github.com/piotrwitek/utility-types))

## Installation

```sh
npm install @adrielus/option
```

(There is also an amd build at `/dist/bundle.amd.js` which uses the `Option` namespace)

## Usage

For detailed usage read [the docs](https://github.com/Mateiadrielrafael/option/tree/master/docs/main.md)

> Note: The docs are still work in progress. Contributions are welcome:)

# Contributing

First, clone this repo:

```sh
git clone https://github.com/Mateiadrielrafael/option
cd option
```

Then use **_pnpm_** to install the dependencies:

```sh
pnpm install
```

You can use the `build` command to build the package (this is dont automatically by github actions):

```sh
pnpm run build
```
