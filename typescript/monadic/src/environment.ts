import { ComponentConfig, Component } from './Component';
import { IterableEmitter } from './iterableEmitter';

export type EnvConfig<T, S, A, O> = {
  render: (template: T, parent: HTMLElement) => void;
  parent: HTMLElement;
  component: ComponentConfig<T, S, A, O, string, {}>;
  initialState: S;
};

// Not an arrow function cause it's a generator
/**
 * Run a component and render it to the dom
 */
export async function* runUi<T, S, A, O>(config: EnvConfig<T, S, A, O>) {
  const reRender = () => config.render(component.getTemplate(), config.parent);

  const outputEmitter = new IterableEmitter<null | O>(null);

  const component = new Component(
    config.initialState,
    config.component,
    outputEmitter.next,
    _ => {
      reRender();
    }
  );

  reRender();

  for await (const event of outputEmitter) {
    if (event) {
      yield event;
    }
  }
}
