import '../styles/reset.scss'

import React from 'react'
import CssBaseline from '@material-ui/core/CssBaseline'

import { BrowserRouter as Router } from 'react-router-dom'
import { theme as MuiTheme } from '../data/Theme'
import { ThemeProvider as Theme } from '@material-ui/styles'
import { AppBar } from './AppBar'
import { Body } from './Body'

export const App = () => {
    return (
        <Theme theme={MuiTheme}>
            <CssBaseline />
            <Router>
                <AppBar />
                <Body />
            </Router>
        </Theme>
    )
}
