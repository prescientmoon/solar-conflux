import '../styles/reset.scss'

import React from 'react'
import CssBaseline from '@material-ui/core/CssBaseline'

import { BrowserRouter as Router } from 'react-router-dom'
import { theme as MuiTheme } from '../data/Theme'
import { ThemeProvider as Theme } from '@material-ui/styles'
import { AppBar } from './AppBar'
import { Body } from './Body'
import { BaseDialogRenderer } from '../../../common/dom/dialogs/components/BaseDialogRenderer'

export const App = () => {
    return (
        <Theme theme={MuiTheme}>
            <CssBaseline />
            <Router>
                <AppBar />
                <Body />
                <BaseDialogRenderer />
            </Router>
        </Theme>
    )
}
