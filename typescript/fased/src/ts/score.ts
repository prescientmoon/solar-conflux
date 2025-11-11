import { html, TemplateResult, render } from "lit-html";

const template = (data: string, name: string = "Score"): TemplateResult => html`<div id="score">${name}:${data}</div>`;
const container = document.getElementById("scoreHolder");
console.log(container);

const display = (score: number, hp: number, element = container) => {
    render(html`
            ${template(score.toFixed(2))}
            ${template(hp.toFixed(2), "Hp")}
        `, element);
}
const die = (score: number, time: number, element = container) => {
    render(html`
            ${template(score.toFixed(2))}
            ${template((time / 1000).toFixed(2), "Time")}
        `, element);
}

export { display, template, container,die };