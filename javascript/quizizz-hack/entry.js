import { render, html } from "https://cdn.skypack.dev/lit-html";

const tags = ["p", "strong", "em", "sub", "sup", "span", "br"];

function removeTags(input) {
  let copy = input.replace('"', '\\"');

  for (const tag of tags)
    copy = copy.replace(`<${tag}>`, "").replace(`</${tag}>`, "");

  return copy;
}

// Here is the function to parse the json object.
// It returns object with key/value pair where key is the question and value is the answer
function parseQuestions(fileObject) {
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

    const questionStr =
      question.structure.query.text === ""
        ? question.structure.query.media[0].url
        : removeTags(question.structure.query.text);

    allAnswers[questionStr] = answer;
  }

  return allAnswers;
}
async function main(id) {
  const res = await fetch(
    `https://api.allorigins.win/get?url=${encodeURIComponent(
      `https://quizizz.com/quiz/${id}`
    )}`,
    {}
  );
  const jsonObject = await res.json();
  const parsedJson = JSON.parse(jsonObject.contents);

  console.log(parsedJson.data);

  const answers = parseQuestions(parsedJson);

  const sorted = Object.entries(answers).sort();

  console.log(answers);

  render(renderQuestions(sorted), rootElement);
}

const inputElement = document.getElementById("id");
const rootElement = document.getElementById("root");

inputElement.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    main(e.target.value).catch(console.error);
  }
});

const renderQuestions = (questions) => {
  return html`${questions.map(([question, answer]) => {
    return html`
      <div class="question-container">
        <div class="question">
          ${question.startsWith("http")
            ? html`<img src=${question} />`
            : question}
        </div>
        ${Array.isArray(answer)
          ? html`
              <ul>
                ${answer.map((correct) => {
                  return html`<li class="answer">${correct}</li> `;
                })}
              </ul>
            `
          : html` <div class="answer answer-single">${answer}</div> `}
      </div>
    `;
  })}`;
};
