import * as Knex from 'knex'

export async function seed(knex: Knex): Promise<any> {
    return knex('account')
        .del()
        .then(() => {
            return knex('account').insert([
                {
                    uid: '1',
                    email: 'rafaeladriel11@gmail.com',
                    name: 'Mock account',
                    description: 'just a mock account',
                    verificationToken: '0123456789',
                    avatar:
                        'https://cdn.vox-cdn.com/thumbor/YuWeAOQKc880Dpo1NYGS1sDBG4A=/1400x1400/filters:format(png)/cdn.vox-cdn.com/uploads/chorus_asset/file/13591799/Screen_Shot_2018_11_30_at_9.47.55_AM.png'
                }
            ])
        })
}
