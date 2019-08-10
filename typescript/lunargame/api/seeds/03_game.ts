import * as Knex from 'knex'

export async function seed(knex: Knex): Promise<any> {
    return knex('game')
        .del()
        .then(() => {
            return knex('game').insert(
                [...Array(300)]
                    .fill(true)
                    .map(() => [
                        {
                            name: 'Spacefilght Simularor',
                            description:
                                'A simulator where you build & fly rockets',
                            public: true,
                            thumbail:
                                'https://is3-ssl.mzstatic.com/image/thumb/Purple118/v4/fa/72/0f/fa720ff4-accb-85de-e558-71b1821399c8/source/512x512bb.jpg'
                        },
                        {
                            name: 'Rocket league',
                            description: 'Basically football - but for cars',
                            public: true,
                            thumbail:
                                'https://steamcdn-a.akamaihd.net/steam/apps/252950/ss_b7e945ac18d86c48b279f26ff6884b5ded2aa1b7.1920x1080.jpg?t=1561064854'
                        },
                        {
                            name: 'Portal',
                            description: 'The cake is a lie',
                            public: true,
                            thumbail:
                                'https://upload.wikimedia.org/wikipedia/en/thumb/9/9f/Portal_standalonebox.jpg/220px-Portal_standalonebox.jpg'
                        },
                        {
                            name: 'Portal 2',
                            description: 'The cake is still a lie',
                            public: true,
                            thumbail:
                                'https://steamcdn-a.akamaihd.net/steam/apps/620/ss_8a772608d29ffd56ac013d2ac7c4388b96e87a21.1920x1080.jpg?t=1512411524'
                        },
                        {
                            name: 'The Stanley parable',
                            description: 'And Stanley... was happy',
                            public: true,
                            thumbail:
                                'https://steamcdn-a.akamaihd.net/steam/apps/221910/ss_49e682563292992309e3047f30128f3dba4c39ce.1920x1080.jpg?t=1465254276'
                        }
                    ])
                    .flat()
            )
        })
}
