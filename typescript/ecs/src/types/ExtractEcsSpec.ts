import { Ecs } from '../classes/Ecs'

export type ExtractEcsSpec<T> = T extends Ecs<infer R> ? R : never
