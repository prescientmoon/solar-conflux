// this is the type wich the node_env constant can take
export type iNode_env = 'development' | 'production' | 'test'

/**
 * Type safe version of process.env.NODE_ENV
 */
export const node_env: iNode_env =
    (process.env.NODE_ENV as iNode_env) || 'development'
