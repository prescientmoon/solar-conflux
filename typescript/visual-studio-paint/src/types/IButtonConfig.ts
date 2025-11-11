import { Painting } from "../classes/Painting"

export interface IButtonConfig {
  icon: string
  text: string
  action?: (painting: Painting) => unknown
}

export interface ISidebarButtonConfig {
  icon: string
  route: string
}
