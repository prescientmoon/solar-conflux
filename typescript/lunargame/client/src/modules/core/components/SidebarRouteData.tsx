import React from 'react'
import HomeIcon from '@material-ui/icons/Home'
import { Route } from '../types/Route'
import { Home } from './Home'

export const routes: Route[] = [
    {
        name: 'home',
        url: '/',
        content: Home,
        icon: <HomeIcon />
    },
    {
        name: 'about',
        url: '/about',
        content: () => (
            <>
                <h1>This is the about component</h1>
            </>
        ),
        icon: <HomeIcon />
    }
]
