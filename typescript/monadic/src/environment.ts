import { ComponentConfig, Component } from './Component';

export type EnvConfig<T, S, A, O> = {
  render: (template: T, parent: HTMLElement) => void;
  parent: HTMLElement;
  component: ComponentConfig<T, S, A, O, string, {}>;
  initialState: S;
};

export const runUi = <T, S, A, O>(config: EnvConfig<T, S, A, O>) => {
  const reRender = () => config.render(component.getTemplate(), config.parent);

  const component = new Component(config.initialState, config.component, _ => {
    reRender();
  });

  reRender();

  return component;
};
