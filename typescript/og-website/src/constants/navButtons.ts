import { UrlConfig } from '../types/UrlConfig'
import { Home } from '../components/Home'
import { Projects } from '../components/Projects'
import { Blog } from '../components/Blog'

export const buttons: UrlConfig[] = [
    {
        url: '/',
        name: 'Home',
        component: Home
    },
    {
        url: '/projects',
        name: 'Projects',
        component: Projects
    },
    {
        url: '/blog',
        name: 'Blog',
        component: Blog
    }
]
