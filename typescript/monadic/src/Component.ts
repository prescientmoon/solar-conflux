import { mapRecord } from './Record';
import { values } from './helpers';

/**
 * Helper type for dispatcher functions
 */
export type Dispatcher<A> = (action: A) => void;

export type ComponentConfig<
  T,
  S,
  A,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  render: (
    state: S,
    dispatch: Dispatcher<A>,
    child: <K extends N>(key: K, name: string, input: C[K]['input']) => T
  ) => T;
  handleAction: (action: A, state: S) => S;
  children: ChildrenTemplates<T, S, N, C>;
};

type GenericLens<T, U> = {
  get: (v: T) => U;
  set: (v: T, n: U) => T;
};

type Child<I = unknown, S = unknown> = {
  input: I;
  state: S;
};

type ChildrenConfigs<N extends string> = Record<N, Child>;

type ChildrenTemplates<T, S, N extends string, C extends ChildrenConfigs<N>> = {
  [K in N]: {
    lens: (input: C[K]['input']) => GenericLens<S, C[K]['state']>;
    component: ComponentConfig<T, C[K]['state'], unknown, string, {}>;
  };
};

type Children<T, S, N extends string, C extends ChildrenConfigs<N>> = {
  [K in N]: Record<
    string,
    {
      component: Component<T, C[K]['state'], unknown, string, {}>;
      lens: GenericLens<S, C[K]['state']>;
    }
  >;
};

export class Component<
  T,
  S,
  A,
  N extends string,
  C extends ChildrenConfigs<N>
> {
  private childrenMap: Children<T, S, N, C>;
  public constructor(
    protected state: S,
    private config: ComponentConfig<T, S, A, N, C>,
    private pushDownwards: (state: S) => void
  ) {
    this.childrenMap = mapRecord(this.config.children, () => ({}));
  }

  protected pushStateUpwards(state: S) {
    this.state = state;

    for (const { component, lens } of this.children()) {
      component.pushStateUpwards(lens.set(state, component.state));
    }
  }

  public getTemplate(): T {
    return this.config.render(
      this.state,
      value => this.dispatch(value),
      (key, name, input) => {
        const hasName = Reflect.has(this.childrenMap[key], name);

        if (!hasName) {
          const lens = this.config.children[key].lens(input);
          const child = this.childrenMap[key] as Record<string, any>;

          child[name] = {
            lens,
            component: new Component(
              lens.get(this.state),
              this.config.children[key].component,
              state => this.pushDownwards(lens.set(this.state, state))
            ),
          };
        }

        return this.childrenMap[key][name].component.getTemplate();
      }
    );
  }

  public dispatch(action: A) {
    const newState = this.config.handleAction(action, this.state);

    this.pushStateUpwards(newState);
    this.pushDownwards(newState);
  }

  /**
   * Get a list of all the children of the component
   */
  private children() {
    return values(this.childrenMap).flatMap(record =>
      values(record)
    ) as Children<T, S, N, C>[N][string][];
  }
}

/**
 * Create a component
 *
 * @param render The render function of the component
 * @param handleAction Function used to handle actions related to the component.
 * @param children Child slots for the component
 */
export const makeComponent = <
  T,
  S,
  A,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  render: ComponentConfig<T, S, A, N, C>['render'],
  handleAction: ComponentConfig<T, S, A, N, C>['handleAction'],
  children: ComponentConfig<T, S, A, N, C>['children']
) => ({ render, handleAction, children });

export const mkChild = <T, PS, I, S>(
  lens: (input: I) => GenericLens<PS, S>,
  component: ComponentConfig<T, S, unknown, string, {}>
) => ({ lens, component });
