import supertest from 'supertest'
import { app } from '../../../server'
import { loggedInAgent } from '../../../../test/utils/loggedInAgent'
import { mockAccounts } from '../../../../test/seeds/01_create-account'
import { random, internet } from 'faker'
import { LoginReponseBody } from '../types/LoginReponseBody'
import { defaultEncryptionMethod } from '../constants'

describe('The /auth route', () => {
    // used to make requests
    let request = supertest(app.callback())

    describe(`The POST method on the /login subroute`, () => {
        test('should throw an error if the password field is empty', async () => {
            const response = await request.post('/auth/login').send({
                name: mockAccounts[0].name
            })

            expect(response.status).not.toBe(200)
        })

        test('should throw an error if the name field is empty', async () => {
            const response = await request.post('/auth/login').send({
                password: mockAccounts[0].password
            })

            expect(response.status).not.toBe(200)
        })

        test('should throw an error if the password is wrong', async () => {
            const response = await request.post('/auth/login').send({
                name: mockAccounts[0].name,
                password: mockAccounts[0].password + 'something'
            })

            expect(response.status).not.toBe(200)
        })

        test('should work just fine when the password is correct', async () => {
            for (const account of mockAccounts) {
                const response = await request.post('/auth/login').send({
                    email: account.email,
                    password: account.password
                })

                // i'm making a separate constant for vsc to help me
                const body: LoginReponseBody = response.body

                expect(response.status).toBe(200)
                expect(body.uid).not.toBe(undefined)
                expect(body.encryption).toBe(account.passwordEncryption)
            }
        })
    })

    describe(`The GET method on the / subroute`, () => {
        test('should return undefined if the user was not logged in', async () => {
            const res = await request.get('/auth')

            expect(res.body.uid).toBe(undefined)
        })

        test('should return the uid form the session while logged in', async () => {
            const [agent, cookie] = await loggedInAgent(supertest.agent(app.callback()), {
                email: mockAccounts[0].email,
                password: mockAccounts[0].password
            })

            const response = await agent.get('/auth').set('cookie', cookie)

            expect(response.body.uid).not.toBe(undefined)
        })
    })

    describe('The POST method on the /signup subroute', () => {
        test('should return the email name and the encrytion', async () => {
            const username = random.alphaNumeric(7)
            const password = random.alphaNumeric(5)
            const email = internet.email()

            const response = await request.post('/auth/signup').send({
                name: username,
                email,
                password
            })

            // i'm making a separate constant for vsc to help me
            const body: LoginReponseBody = response.body

            expect(response.status).toBe(200)
            expect(body.uid).not.toBe(undefined)
            expect(body.encryption).toBe(defaultEncryptionMethod)
        })
    })
})
