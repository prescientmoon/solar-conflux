import React, { Component } from 'react'

export interface Route {
    name: string
    url: string
    content: (props: unknown) => JSX.Element
    icon: JSX.Element
}
