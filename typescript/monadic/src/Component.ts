import { mapRecord } from './Record';
import { values, GenericLens } from './helpers';
import O from 'fp-ts/es6/Option';
import { constant } from 'fp-ts/es6/function';

/**
 * Helper type for dispatcher functions
 */
export type Dispatcher<A> = (action: A) => void;

export type Communicate<
  T,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  dispatch: Dispatcher<A>;
  child: <K extends N>(key: K, name: string, input: C[K]['input']) => T;
  raise: (event: O) => void;
};

export type RenderFunction<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = (state: S, communicate: Communicate<T, A, O, N, C>) => T;

export type ComponentConfig<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = {
  render: RenderFunction<T, S, A, O, N, C>;
  handleAction: (action: A, state: S) => S;
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
    private pushDownwards: (state: S) => void
  ) {
    this.childrenMap = mapRecord(this.config.children, constant({}));
  }

  protected pushStateUpwards(state: S) {
    this.state = state;

    for (const { component, lens } of this.children()) {
      component.pushStateUpwards(lens.set(state, component.state));
    }
  }

  public getTemplate(): T {
    return this.config.render(this.state, {
      dispatch: value => this.dispatch(value),
      raise: _ => undefined,
      child: (key, name, input) => {
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
      },
    });
  }

  public dispatch(action: A) {
    const newState = this.config.handleAction(action, this.state);

    this.pushStateUpwards(newState);
    this.pushDownwards(newState);
  }

  /**
   * Get a list of all the children of the component
   */
  private children(): this['childrenMap'][N][string][] {
    return values(this.childrenMap).flatMap(record => values(record)) as any;
  }
}
