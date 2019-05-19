import * as morgan from "morgan"
import chalk from "chalk"

const { floor } = Math

export const morganChalk = morgan(function (tokens, req, res) {
    const status = Number(tokens.status(req,res))
    const statusFirstDigit = floor(status / 100)
    
    const emoji = (status === 200) ? 
        "👌" : (status === 203) ?
        "🔎" : (status == 202) ?
        "🚧" : (status === 302) ?
        "🏹" : (statusFirstDigit === 2) ?
        "🌝" : (statusFirstDigit === 4) ?
        "😠" : 
        "❓"
    
    return [
        chalk.bold(`${emoji} `),
        chalk.green.bold(tokens.method(req, res)),
        chalk.red.bold(tokens.status(req, res)),
        chalk.blue.italic(tokens.url(req, res)),
        chalk.yellow(`${tokens['response-time'](req, res)} ms`),
    ].join(' ');
});