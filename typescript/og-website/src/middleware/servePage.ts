import { Request, Response } from 'express'
import { createComponentRenderer } from '../helpers/renderComponent'
import { LayoutOptions } from '../types/LayoutOptions'

export const createPageMiddlewareFactory = (
    ...args: Parameters<typeof createComponentRenderer>
) => {
    const renderComponent = createComponentRenderer(...args)
    return (config: LayoutOptions) => (
        request: Request,
        response: Response
    ) => {
        console.log(`Serving page ${config.title}`)
        renderComponent(response, config)
    }
}
