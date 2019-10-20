import { html } from '@popeindustries/lit-html-server'
import { ProjectConfig } from '../types/Project'
import { withCss } from '../helpers/withCss'
import { projects } from '../constants/projects'

export const Project = (project: ProjectConfig) => {
    const style = `background-image: url(/static/assets/${project.thumbail})`
    return html`
        <div class="project">
            <a href=${project.demo || project.docs || project.source}>
                <div class="project-thumbail background" style=${style}></div>
            </a>
            <div class="project-name">${project.name}</div>
            <div class="project-links">
                <a class="project-source" href=${project.source}>Source</a>

                ${project.demo
                    ? html`
                          <a class="project-demo" href=${project.demo}>Demo</a>
                      `
                    : ''}
                ${project.docs
                    ? html`
                          <a class="project-demo" href=${project.docs}>Docs</a>
                      `
                    : ''}
            </div>
        </div>
    `
}

export const Projects = () => html`
    ${withCss('projects')}
    <div id="projects" class="full center">
        ${projects.map(Project)}
    </div>
`
