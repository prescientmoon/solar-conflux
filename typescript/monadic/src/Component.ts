import { mapRecord } from './Record';
import { values, GenericLens, areEqual } from './helpers';
import * as O from 'fp-ts/es6/Option';
import { constant, flow } from 'fp-ts/es6/function';

/**
 * Helper type for dispatcher functions
 */
export type Dispatcher<A> = (action: A) => void;

export type Communicate<A, O> = {
  dispatch: Dispatcher<A>;
  raise: (event: O) => void;
};

export type CanAddChildren<
  T,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  child: <K extends N>(key: K, name: string, input: C[K]['input']) => T;
};
export type RenderFunction<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = (state: S, communicate: Communicate<A, O> & CanAddChildren<T, N, C>) => T;

export type ComponentConfig<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  render: RenderFunction<T, S, A, O, N, C>;
  handleAction: (action: A, state: S, communication: Communicate<A, O>) => S;
  children: ChildrenTemplates<T, S, A, N, C>;
};

export type Child<I = unknown, S = unknown, O = unknown> = {
  input: I;
  state: S;
  output: O;
};

export type ChildrenConfigs<N extends string> = Record<N, Child>;

export type ChildTemplate<T, PS, PA, S, I, O> = {
  lens: (input: I) => GenericLens<PS, S>;
  component: ComponentConfig<T, S, unknown, O, string, {}>;
  handleOutput: (input: I, output: O) => O.Option<PA>;
};

type ChildrenTemplates<
  T,
  S,
  A,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  [K in N]: ChildTemplate<
    T,
    S,
    A,
    C[K]['state'],
    C[K]['input'],
    C[K]['output']
  >;
};

type Children<T, S, N extends string, C extends ChildrenConfigs<N>> = {
  [K in N]: Record<
    string,
    {
      component: Component<
        T,
        C[K]['state'],
        unknown,
        C[K]['output'],
        string,
        {}
      >;
      lens: GenericLens<S, C[K]['state']>;
    }
  >;
};

export class Component<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> {
  public childrenMap: Children<T, S, N, C>;
  public constructor(
    protected state: S,
    private config: ComponentConfig<T, S, A, O, N, C>,
    private raise: Communicate<A, O>['raise'],
    private pushDownwards: (state: S) => void
  ) {
    this.childrenMap = mapRecord(this.config.children, constant({}));
  }

  /**
   * Do something only if the state changed.
   *
   * @param state The new state.
   * @param then The thing you want to do with the state
   */
  private setStateAndThen = (then: (state: S) => unknown) => (state: S) => {
    if (areEqual(state, this.state)) {
      return;
    }

    this.state = state;

    then(state);
  };

  /**
   * Propagate a new state upstream the tree
   *
   * @param state The new state
   */
  protected pushStateUpwards = this.setStateAndThen(state => {
    for (const { component, lens } of this.children()) {
      component.pushStateUpwards(lens.get(state));
    }
  });

  /**
   * Function to get a child or create it if it doesn't exist
   */
  private getChild: CanAddChildren<T, N, C> = {
    child: (key, name, input) => {
      const hasName = Reflect.has(this.childrenMap[key], name);

      if (!hasName) {
        const config = this.config.children[key];
        const lens = config.lens(input);
        const raise = flow(config.handleOutput, O.map(this.dispatch));
        const child = this.childrenMap[key] as Record<string, any>;

        child[name] = {
          lens,
          component: new Component(
            lens.get(this.state),
            this.config.children[key].component,
            event => raise(input, event),
            childState => {
              const newState = lens.set(this.state, childState);

              this.setStateAndThen(this.pushDownwards)(newState);
            }
          ),
        };
      }

      return this.childrenMap[key][name].component.getTemplate();
    },
  };

  private getCommunication(): Communicate<A, O> {
    return {
      dispatch: value => this.dispatch(value),
      raise: this.raise,
    };
  }

  public getTemplate(): T {
    return this.config.render(this.state, {
      ...this.getCommunication(),
      ...this.getChild,
    });
  }

  /**
   * Dispatch an arbitrary action on the component.
   */
  public dispatch = (action: A) => {
    const newState = this.config.handleAction(
      action,
      this.state,
      this.getCommunication()
    );

    this.pushStateUpwards(newState);
    this.pushDownwards(newState);
  };

  /**
   * Get a list of all the children of the component
   */
  private children(): this['childrenMap'][N][string][] {
    return values(this.childrenMap).flatMap(record => values(record)) as any;
  }
}
