import {
  ComponentConfig,
  ChildrenConfigs,
  ChildTemplate,
  RenderFunction,
  Child,
} from './Component';
import { GenericLens } from './helpers';
import { constant, identity } from 'fp-ts/es6/function';
import * as O from 'fp-ts/es6/Option';

type ComponentBuilder = <
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  render: ComponentConfig<T, S, A, O, N, C>['render'],
  handleAction: ComponentConfig<T, S, A, O, N, C>['handleAction'],
  children: ComponentConfig<T, S, A, O, N, C>['children']
) => ComponentConfig<T, S, A, O, N, C>;

/**
 * Create a component
 *
 * @param render The render function of the component
 * @param handleAction Function used to handle actions related to the component.
 * @param children Child slots for the component
 */
export const makeComponent: ComponentBuilder = (
  render,
  handleAction,
  children
) => ({ render, handleAction, children });

type ChildBuilder = <T, PS, PA, S, I, O>(
  lens: (input: I) => GenericLens<PS, S>,
  component: ComponentConfig<T, S, unknown, O, string, {}>,
  handleOutput?: (input: I, output: O) => O.Option<PA>
) => ChildTemplate<T, PS, PA, S, I, O>;

export const mkChild: ChildBuilder = (
  lens,
  component,
  handleOutput = constant(O.none)
) => ({ lens, component, handleOutput });

export type ComponentSpec<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = [T, S, A, O, N, C];

export const withChild = constant(identity) as <
  N1 extends keyof any,
  I,
  S,
  O
>() => <T, PS, A, PO, N0 extends string, C extends ChildrenConfigs<N0>>(
  spec: ComponentSpec<T, PS, A, PO, N0, C>
) => ComponentSpec<T, PS, A, PO, N1 & N0, C & Record<N1, Child<I, S, O>>>;

export const withAction = constant(identity) as <A1>() => <
  T,
  S,
  A0,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T, S, A0, O, N, C>
) => ComponentSpec<T, S, A1, O, N, C>;

export const withState = constant(identity) as <S1>() => <
  T,
  S0,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T, S0, A, O, N, C>
) => ComponentSpec<T, S1, A, O, N, C>;

export const withTemplate = constant(identity) as <T1>() => <
  T0,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T0, S, A, O, N, C>
) => ComponentSpec<T1, S, A, O, N, C>;

export const buildSpec = (constant(makeComponent) as any) as <
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  render: RenderFunction<T, S, A, O, N, C>,
  handleAction: ComponentConfig<T, S, A, O, N, C>['handleAction'],
  children: ComponentConfig<T, S, A, O, N, C>['children']
) => (
  spec: ComponentSpec<T, S, A, O, N, C>
) => ComponentConfig<T, S, A, O, N, C>;

export const emptySpec = (null as any) as ComponentSpec<
  unknown,
  unknown,
  unknown,
  null,
  string,
  {}
>;
