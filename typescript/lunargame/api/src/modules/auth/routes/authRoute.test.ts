import supertest from 'supertest'
import { app } from '../../../server'
import { loggedInAgent } from '../../../../test/utils/loggedInAgent'
import { mockAccounts } from '../../../../test/seeds/01_create-account'
import { random, internet } from 'faker'
import { defaultEncryptionMethod } from '../constants'

describe('The /auth route', () => {
    // used to make requests
    const request = supertest(app.callback())

    describe(`The GET method on the / subroute`, () => {
        test('should return undefined if the user was not logged in', async () => {
            // act
            const res = await request.get('/auth')

            // assert
            expect(res.body.uid).toBe(undefined)
        })

        test('should return the uid form the session while logged in', async () => {
            // arrange
            const [agent, cookie] = await loggedInAgent(supertest.agent(app.callback()), {
                email: mockAccounts[0].email,
                password: mockAccounts[0].password
            })

            // act
            const response = await agent.get('/auth').set('cookie', cookie)

            // assert
            expect(response.body.uid).not.toBe(undefined)
        })
    })

    describe(`The POST method on the /login subroute`, () => {
        test('should throw an error if the user is already logged in', async () => {
            // arrange
            const [agent, cookie] = await loggedInAgent(supertest.agent(app.callback()), {
                email: mockAccounts[0].email,
                password: mockAccounts[0].password
            })

            // act
            const reponse = await agent.post('/auth/login').set('cookie', cookie)

            // assert
            expect(reponse.status).toBe(401)
        })

        test('should throw an error if the password is wrong', async () => {
            // act
            const response = await request.post('/auth/login').send({
                email: mockAccounts[0].email,
                password: mockAccounts[0].password + 'something'
            })

            // assert
            expect(response.status).toBe(422)
            expect((response.body.message as string).startsWith('child')).toBe(false) // Not JOI
        })

        test("should throw an error if the user doesn't exist", async () => {
            // act
            const reponse = await request.post('/auth/login').send({
                email: 'idk' + mockAccounts[0].email,
                password: mockAccounts[0].password
            })

            // assert
            expect(reponse.status).toBe(404)
        })

        test('should work when the password is correct', async () => {
            for (const account of mockAccounts) {
                // act
                const response = await request.post('/auth/login').send({
                    email: account.email,
                    password: account.password
                })

                // assert
                expect(response.status).toBe(200)
                expect(response.body.uid).not.toBe(undefined)
                expect(response.body.encryption).toBe(account.passwordEncryption)
            }
        })
    })

    describe('The POST method on the /signup subroute', () => {
        test('should work if all fields are correct', async () => {
            // arrange
            const name = internet.userName()
            const password = random.alphaNumeric(5)
            const email = internet.email()

            const user = {
                name,
                email,
                password
            }

            // act
            const response = await request.post('/auth/signup').send(user)

            // assert
            expect(response.status).toBe(200)
            expect(response.body.uid).not.toBe(undefined)
            expect(response.body.encryption).toBe(defaultEncryptionMethod)
        })
    })
})
