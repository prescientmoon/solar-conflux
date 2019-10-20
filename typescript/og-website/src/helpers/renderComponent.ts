import { TemplateResult, renderToStream } from '@popeindustries/lit-html-server'
import { LayoutOptions } from '../types/LayoutOptions'
import { Response } from 'express'

export const createComponentRenderer = (
    layout: (options: LayoutOptions) => TemplateResult,
    name: string
) => (res: Response, { body, title, url }: LayoutOptions) => {
    renderToStream(
        layout({
            body,
            url,
            title: `${name} | ${title}`
        })
    ).pipe(res)
}
