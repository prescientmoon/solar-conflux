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
        renderComponent(response, config)
    }
}
