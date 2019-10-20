import { TemplateResult } from '@popeindustries/lit-html-server'

export interface LayoutOptions {
    title: string
    url: string
    body: TemplateResult | string
}
