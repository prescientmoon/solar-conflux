import { IterableEmitter } from './iterableEmitter';

export type EnvConfig<T, S, A> = {
  render: (template: T, parent: HTMLElement) => void;
  parent: HTMLElement;
  component: Component<T, S, A>;
  initialState: S;
};

export type Component<T, S, A> = {
  render: (state: S, dispatch: (a: A) => () => void) => T;
  handleActions: (action: A, state: S) => S;
};

async function* runComponent<T, S, A>(
  component: Component<T, S, A>,
  initialState: S
): AsyncGenerator<T> {
  const emitter = new IterableEmitter(initialState);

  const dispatch = (state: S) => (action: A) => {
    const newState = component.handleActions(action, state);

    return () => {
      emitter.next(newState);
    };
  };

  for await (const state of emitter) {
    yield component.render(state, dispatch(state));
  }
}

export const runUi = async <T, S, A>(
  config: EnvConfig<T, S, A>
): Promise<void> => {
  const component = runComponent(config.component, config.initialState);

  for await (const template of component) {
    config.render(template, config.parent);
  }
};
