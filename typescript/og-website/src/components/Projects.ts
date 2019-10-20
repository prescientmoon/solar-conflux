import { html } from '@popeindustries/lit-html-server'
import { ProjectConfig } from '../types/Project'
import { withCss } from '../helpers/withCss'
import { projects } from '../constants/projects'

export const Project = (project: ProjectConfig) => {
    const style = `background-image: url(/static/assets/${project.thumbail})`
    return html`
        <div class="project">
            <div class="project-thumbail background" style=${style}></div>
            <div class="project-icons">
                <a class="project-source" href=${project.source}>Source</a>
                <a class="project-demo" href=${project.demo}>Demo</a>
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
