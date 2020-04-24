import { Component, runComponent } from './component';

export type EnvConfig<T, S, A> = {
  render: (template: T, parent: HTMLElement) => void;
  parent: HTMLElement;
  component: Component<T, S, A>;
  initialState: S;
};

export const runUi = async <T, S, A>(
  config: EnvConfig<T, S, A>
): Promise<void> => {
  const component = runComponent(config.component, config.initialState);

  for await (const template of component) {
    config.render(template, config.parent);
  }
};
