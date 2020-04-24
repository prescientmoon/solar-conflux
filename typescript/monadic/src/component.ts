import { IterableEmitter } from './iterableEmitter';

export type Component<T, S, A> = {
  render: (state: S, dispatch: (a: A) => () => void) => T;
  handleActions: (action: A, state: S) => S;
};

export async function* runComponent<T, S, A>(
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
