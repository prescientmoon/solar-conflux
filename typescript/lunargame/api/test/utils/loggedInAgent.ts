import supertest from 'supertest'

/**
 * Helper to get a supertest agent wich is logged in
 *
 * @param agent The agent to make the request with
 * @param uid The uid to use to login
 */
export const loggedInAgent = async (
    agent: supertest.SuperTest<supertest.Test>,
    uid: number
) => {
    const response = await agent.post('/auth/login').send({
        uid
    })

    // the cookie to send back
    // needs to be set manually due to a bug with jest
    const cookie = response.header['set-cookie']

    expect(response.status).toBe(200)

    return [agent, cookie]
}
