import supertest from 'supertest'
import { app } from '../../../server'
import { loggedInAgent } from '../../../../test/utils/loggedInAgent'

describe('The /auth route', () => {
    let request = supertest(app.callback())

    test('should return undefined if the user was not logged in', async () => {
        const res = await request.get('/auth')

        expect(res.body.uid).toBe(undefined)
    })

    test.only('should return the uid form the session while logged in', async () => {
        const uid = 7

        const [agent, cookie] = await loggedInAgent(
            supertest.agent(app.callback()),
            uid
        )

        const res = await agent.get('/auth').set('cookie', cookie)

        expect(res.body.uid).toBe(uid)
    })
})
