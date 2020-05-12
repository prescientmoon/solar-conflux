import {
  ComponentConfig,
  ChildrenConfigs,
  ChildTemplate,
  RenderFunction,
  Child,
} from './Component';
import { GenericLens } from './helpers';
import { constant, identity, flow } from 'fp-ts/es6/function';
import * as O from 'fp-ts/es6/Option';

// This is here since all the builders are pretty much this
const alwaysIdentity = constant(identity);

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

/**
 * Structure which keeps track of the different types needed to build a component.
 */
export type ComponentSpec<
  T,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
> = [T, S, A, O, N, C];

/**
 * Add a child to a spec (type only, the implementation is given when building the spec)
 */
export const withChild = alwaysIdentity as <
  N1 extends keyof any,
  I,
  S,
  O
>() => <
  T,
  PS,
  A,
  PO,
  N0 extends Exclude<string, N1>,
  C extends ChildrenConfigs<N0>
>(
  spec: ComponentSpec<T, PS, A, PO, N0, C>
) => ComponentSpec<T, PS, A, PO, N1 & N0, C & Record<N1, Child<I, S, O>>>;

/**
 * Specify the action type of a spec
 */
export const withAction = alwaysIdentity as <A1>() => <
  T,
  S,
  A0,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T, S, A0, O, N, C>
) => ComponentSpec<T, S, A1, O, N, C>;

/**
 * Specify the output type of a spec
 */
export const withOutput = alwaysIdentity as <O1>() => <
  T,
  S,
  A,
  O0,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T, S, A, O0, N, C>
) => ComponentSpec<T, S, A, O1, N, C>;

/**
 * Specify the type of state in a spec
 */
export const withState = alwaysIdentity as <S1>() => <
  T,
  S0,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T, S0, A, O, N, C>
) => ComponentSpec<T, S1, A, O, N, C>;

/**
 * Specify what a component renders to
 */
export const withTemplate = alwaysIdentity as <T1>() => <
  T0,
  S,
  A,
  O,
  N extends string,
  C extends ChildrenConfigs<N>
>(
  spec: ComponentSpec<T0, S, A, O, N, C>
) => ComponentSpec<T1, S, A, O, N, C>;

/**
 * Takes implementations of the components watching the spec and creates the component
 */
export const buildSpec = flow(makeComponent, constant) as <
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
