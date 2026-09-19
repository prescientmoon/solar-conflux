import { Ecs } from '../classes/Ecs'

export type ExtractComponentType<
  E,
  K extends E extends Ecs<infer R> ? keyof E['components'] : never
> = E extends Ecs<infer R> ? R[K] : never
