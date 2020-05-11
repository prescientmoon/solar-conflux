import { ComponentConfig, Component } from './Component';

export type EnvConfig<T, S, A> = {
  render: (template: T, parent: HTMLElement) => void;
  parent: HTMLElement;
  component: ComponentConfig<T, S, A, string, {}>;
  initialState: S;
};

export const runUi = <T, S, A>(config: EnvConfig<T, S, A>) => {
  const reRender = () => config.render(component.getTemplate(), config.parent);

  const component = new Component(config.initialState, config.component, _ => {
    reRender();
  });

  reRender();

  return component;
};
