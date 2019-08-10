type processMode = 'development' | 'production' | 'test'

export const mode: processMode =
    (process.env.NODE_ENV as processMode) || 'development'
