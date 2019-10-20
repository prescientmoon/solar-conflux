import { TemplateResult } from '@popeindustries/lit-html-server'

export interface UrlConfig {
    url: string
    name: string
    component: () => TemplateResult
}
