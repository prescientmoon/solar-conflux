# Monadic

Random functional-ish web framework thing I made for fun.

# Small tutorial

## Templates

This library lets you render to any kind of template. For this tutorial we'll be using lit-html.

Components are created using specs. Let's create a base spec which says "render this component to a lit-html template result":

```ts
import { withTemplate } from '../../src';
import { TemplateResult } from 'lit-html';
import { pipe } from 'fp-ts/lib/pipeable';

// Pipe is a helper from fp-ts which is the equivalent of the pipe operator proposal as a function
const baseSpec = pipe(emptySpec, withTemplate<TemplateResult>());
```

## Components

The most important thing a component has is state. To specify the the type of a component' state use the withState builder:

```ts
type State = { count: number };

const counterComponent = pipe(baseSpec, withState<State>());
```

State is modified using actions. First we need a type for the action. For our counter component our only possible actions are increasing and decreasing the state:

```ts
type Action = 'increase' | 'decrease';

const counterComponent = pipe(
  baseSpec,
  withState<State>(),
  withAction<Action>()
);
```

## Building specs

To build a spec you need to use the `buildSpec` function. BuildSpec takes 3 arguments:

- The render function: takes some sate and generates a lit-html template
- The handleAction function: takes some state and an action and returns some new state
- Children: we'll cover this later. For now we'll just pass {} to it

Let's create a small render function for our counter component:

```ts
const counterComponent = pipe(
  baseSpec,
  withState<State>(),
  withAction<Action>(),
  buildSpec(
    ({ count }) => {
      return html`
        <div>
          Count: ${count}
        </div>
      `;
    },
    (action, state) => {
      // just return the state for now
      return state;
    },
    {}
  )
);
```

The render function also takes a second argument exposing some functions to the component. For now let's take a look at dispatch. Dispatch takes an action and calls your handleAction function replacing the component state with the return of that function.

Let's implement the basic actions for our component

```ts
const counterComponent = pipe(
  ...buildSpec(
    ({ count }, { dispatch }) => {
      return html`
        <div>
          Count: ${count}
          <button @click=${() => dispatch('increase')}>Increase</button>
          <button @click=${() => dispatch('decrease')}>Decrease</button>
        </div>
      `;
    },
    (action, { count }) => {
      if (action === 'increase') {
        return { count: count + 1 };
      }

      return { count: count - 1 };
    },
    {}
  )
);
```

## Running a component

To run a component simply use the runUi function:

```ts
runUi({
    parent: document.getElementById('app') // you can use any dom element here,
    render, // this is a function which takes the parent and a template and renders it. In this case we use the render function from lit-html
    initialState: { count: 0 }, // this is the state your component starts out with
    component: counterComponent, // the component to render
});
```

Now if you run your app with something like parcel you should see a working counter!

## Children

In my library all the state is always in sync. To do that we need to explain monadic how to sync it. This is what the last argument of buildSpec if for. Let's define a simple counters component which keeps a list of counters:

```ts
type CounterListState = {
  counters: CounterState[]; // We renamed our previous State type to CounterState to prevent confusion
};

const counterList = pipe(
  baseSpec,
  withState<CounterState>(),
  buildSpec(
    ({ counters }) => {
      return html`
        <div>
          Counters go here
        </div>
      `;
    },
    (_, state) => {
      // We have no actions for now so we can just return the state
      return state;
    },
    {}
  )
);
```

Using components inside another components:

1. You need to explain typescript what your components looks like. To do this you need to use the withChild state builder which takes 4 arguments:
   - The name of the component
   - The input your state syncing function takes. In our case this will be just the index of the counter.
   - The state your child component has
   - The output your components raises. We'll talk about this one later

Let's add our counter component to the spec of our counterList component:

```ts
const counterList = pipe(
  baseSpec,
  withState<CounterState>(),
  withChild<"counter", number, CounterState, null>()
  ...
```

## Syncing states

To sync a parent and a child state we need 2 functions:

- Get: takes the parent state and returns the child state
- Set: takes a parent and a child state and merges them

Let's see this in action by passing an actual child to the 3rd argument of buildSpec:

```ts
buildSpec(
  ...,{
    // This has to match the name you passed to withChild
    counter: mkChild(
      // We told withChild our component takes an input of type number being the index of the counter
      index => ({
        // Our
        get: ({ counters }) => counters[index],
        set: (state, newCounterState) => {
            ...state,
            // This function just returns an array with an index modified. Try implementing it yourself :D
            counters: setIndex(state.counters, index, newCounterState)
        }
      }),
      counterComponent // This is the actual component we created earlier
      // There's also an optional 3rd parameter for outputs but we'll cover that later
    )
  }
);
```

Now we can include the component in our render function:

```ts
({ counters }, { child }) => {
  return html`
    <div>
      This is a counter list with ${counters.length} counters inside it!
      <div>
        ${counters.map((_, index) =>
          child(
            // This is the name of the counter. It has to match what you passed to withChild
            'counter',
            // This is similar to key in react. It's used to keep track of the state of each counter.
            // Using the index is usually bad practice
            // You should probably use something like an id for it if you can
            String(index),
            // This is the input we pass to our sync functions. It has to match what we gave to withChild
            index
          )
        )}
      </div>
    </div>
  `;
};
```

Now run your component with some initial state and you should see a counter element for each counter in the state!

> Exercise: add an input element and a button allowing the creation of new counters

## Output

Outputs are like events. Let's say we want counters to have a "delete" button. To do that we need to allow the counter component to send messages to the counterList component. Let's start by defining the type of our messages:

```ts
type CounterOutput = 'delete';
```

Then let's edit the counter component to emit the output on click. For this the `raise` function is used:

```ts
const counterComponent = pipe(
  ...,
  withOutput<CounterOutput>(),
  buildSpec(
    ({ count }, { dispatch, raise }) => {
      return html`
        <div>
          <button @click=${() => raise("delete")}> Delete this counter </button>
          Count: ${count}
          <button @click=${() => dispatch('increase')}>Increase</button>
          <button @click=${() => dispatch('decrease')}>Decrease</button>
        </div>
      `;
    },
    ...
  )
);
```

And now we need to update the `withChild` builder from the list component to reflect the changes:

```ts
...
withChild<"counter", number, CounterState, CounterOutput>()
...
```

Now we need to actually handle the outputs. You don't always want to do something with an output, so monadic allows you do discard any output you want. To handle the output -> action relation `mkChild` accepts an extra argument which takes an output and returns:

```ts
Option<Action>
```

where Option is the Option type from fp-ts.

Let's create a new action in our list component which deletes a counter based on it's index:

```ts
type CounterAction = {
  label: 'delete';
  index: number;
};

const counterList = pipe(
  ...,
  withAction<CounterAction>(),
  buildSpec(
    ...,
    (action, state) => {
      if (action.label === 'delete') {
        return {
          ...state,
          counters: state.counters.filter((_, index) => index !== action.index),
        };
      }
    }
  )
);
```

And now let's define the function which handles the outputs:

```ts
buildSpec(
  ...,{
    counter: mkChild(
        ...,
        (index, output) => output === "delete" ? O.some({ label: "delete", index }) : O.none
    )
  }
);
```

And voila! Now you should be able to delete any counter!

## Listening to outputs on root:

Our counter list doesn't have any outputs currently, but let's assume it had the following set of outputs:

```ts
type CounterListOutputs =
  | { label: 'foo'; value: number }
  | { label: 'bar'; value: string };
```

We can listen to outputs on our root component with a simple for loop:

```ts
const main = async () => {
  // runUi returns an async iterator
  const output = runUi(...);

  for await (const event of output) {
      if (event.label === "foo") {
          console.log(`Received a foo event with a number payload of ${event.value}`)
      }
      else if (event.label === "bar") {
          console.log(`Received a bar event with a string payload of ${event.value}`)
      }
  }
};

main().catch(err => {
    // Handle errors
});
```

## Closing notes:

You access `dispatch` and `raise` inside your action handler as well:

```ts
(action, state, { dispatch, raise }) => {
    ...
}
```
