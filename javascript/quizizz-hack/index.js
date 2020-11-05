const fetch = require("node-fetch");
const chalk = require("chalk");
const { default: readline } = require("readline-promise");

const id = process.argv[2];

const rlp = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: true,
});

const tags = ["p", "strong", "em"];

function removeTags(input) {
  let copy = input.replace('"', '\\"');

  for (const tag of tags)
    copy = copy.replace(`<${tag}>`, "").replace(`</${tag}>`, "");

  return copy;
}

// Here is the function to parse the json object.
// It returns object with key/value pair where key is the question and value is the answer
function parseFile(fileObject) {
  const allAnswers = {};

  for (const question of fileObject.data.quiz.info.questions) {
    let answer;

    if (question.type === "MCQ") {
      if (question.structure.options[question.structure.answer].text == "") {
        answer =
          question.structure.options[question.structure.answer].media[0].url;
      } else {
        answer = removeTags(
          question.structure.options[question.structure.answer].text
        );
      }
    } else if (question.type == "MSQ") {
      answer = question.structure.answer
        .map((answerC) => {
          if (question.structure.options[answerC].text == "") {
            return question.structure.options[answerC].media[0].url;
          } else {
            return removeTags(question.structure.options[answerC].text);
          }
        })
        .filter((a) => a !== undefined);
    } else if (question.type == "BLANK") {
      answer = question.structure.options.map((o) => removeTags(o.text));
    }

    const questionStr = removeTags(question.structure.query.text);

    allAnswers[questionStr] = answer;
  }

  return allAnswers;
}

function printQuestion(question) {
  console.log(chalk.italic(chalk.blue(question[0])));
  console.log(chalk.green(question[1]));
}

async function repl(o) {
  const answer = await rlp.questionAsync(">");

  if (answer === "all") {
    for (const question of o) {
      printQuestion(question);
    }
  } else {
    console.log(chalk.underline(answer));

    for (const question of o) {
      let ok = true;

      for (let i = 0; i < question[0].length; i++) {
        if (answer[i] === undefined) break;

        if (question[0][i] !== answer[i]) {
          ok = false;
          break;
        }
      }

      if (ok) {
        printQuestion(question);
      }
    }
  }

  repl(o);
}

async function main() {
  const res = await fetch(`https://quizizz.com/quiz/${id}`);
  const jsonObject = await res.json();
  const answers = parseFile(jsonObject);

  const sorted = Object.entries(answers).sort();

  repl(sorted);
}

main();
