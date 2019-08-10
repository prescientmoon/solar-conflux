export const subject = 'Lunarbox verification'
export const text = (token: string, name: string) => `
    Hey ${name}! Welcome to lunarbox! To verify your email, click the link bellow: ${
    process.env.SERVER_URL
}/account/verify/${token}
`
