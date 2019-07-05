import React from 'react'
import { useObservable } from 'rxjs-hooks'
import { interval } from 'rxjs'
import { map } from 'rxjs/operators'

export const App = () => {
    const somenum = useObservable<number>($input => {
        return interval(1000).pipe(map(val => val + 1))
    }, 200)

    return (
        <>
            <h1>This is the app component!</h1>
            <span>some random rxjs counter => {somenum}</span>
        </>
    )
}
