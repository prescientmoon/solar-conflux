import React from 'react'
import HomeIcon from '@material-ui/icons/Home'
import GamesIcon from '@material-ui/icons/Games'
import { Route } from '../types/Route'
import { Home } from './Home'
import { Games } from '../../games/components/GamePage'

export const routes: Route[] = [
    {
        name: 'home',
        url: '/',
        content: Home,
        icon: <HomeIcon />
    },
    {
        name: 'games',
        url: '/games',
        content: Games,
        icon: <GamesIcon />
    }
]
