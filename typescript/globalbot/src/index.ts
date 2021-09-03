import TwitterApi, { TweetV2 } from "twitter-api-v2";
import { config } from "dotenv";

config();

const api = new TwitterApi(process.env.TOKEN);

const luke = "GlobeGames1";
const latest = "1433459713035935749";
const command = "!vote";
const prefixes = [command, `@${luke}`];

const client = api.readOnly;

const { aliases, countries } = require("./countries.json");

const getCoutntryName = (message: string): string | null => {
  const trimmed = message.trim().toLowerCase();

  for (const prefix of prefixes)
    if (trimmed.startsWith(prefix.toLowerCase()))
      return getCoutntryName(trimmed.substr(prefix.length));

  for (const alias in aliases) {
    if (trimmed.startsWith(alias.toLowerCase())) return aliases[alias];
  }

  for (const country of countries) {
    if (trimmed.startsWith(country.toLowerCase())) return country;
  }

  return null;
};

const main = async () => {
  const data: TweetV2[] = [];

  let token: string | undefined;
  let fetches = 0;

  do {
    const tweets = await client.v2.search(
      `to:${luke} conversation_id:${latest}`,
      {
        "tweet.fields": ["text", "author_id", "referenced_tweets"],
        next_token: token,
      }
    );

    data.push(...tweets.data.data);
    token = tweets.meta.next_token;
    fetches++;
  } while (token);

  console.log(`Fetched the twitter api ${fetches} times`);

  const votes = new Map();

  for (const vote of data) {
    if (
      !vote.referenced_tweets.some(
        (tweet) => tweet.type === "replied_to" && tweet.id === latest
      )
    )
      continue;

    const country = getCoutntryName(vote.text);

    if (country === null) continue;

    votes.set(vote.author_id, country);
  }

  const voteResults = new Map();

  for (const [_author, country] of votes) {
    const previousAmount = voteResults.get(country);

    voteResults.set(country, (previousAmount ?? 0) + 1);
  }

  const sortedResults = [...voteResults.entries()];
  sortedResults.sort((a, b) => b[1] - a[1]);

  console.log(Object.fromEntries(sortedResults));
};

main().catch((e) => {
  process.exit(1);
});
