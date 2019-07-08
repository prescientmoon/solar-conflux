import React, { Component } from 'react'

export type acceptedContent = React.ComponentElement<unknown, Component>

export interface Route {
    name: string
    url: string
    content: () => acceptedContent
    icon: acceptedContent
}
