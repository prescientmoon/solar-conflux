import { Some } from '../src'

// general value to pass around
export const x = Symbol('x')

// same as x but for some
export const someX = Some(x)
