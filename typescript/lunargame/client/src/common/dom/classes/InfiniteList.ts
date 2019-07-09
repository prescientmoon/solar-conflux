import { cacheInstances } from '../../lang/objects/decorators/cacheInstances'
import { BehaviorSubject } from 'rxjs'
import { BaseServer } from '../../../modules/network/classes/BaseServer'

export interface InfiniteListConfig {
    urls: {
        chunk: string
        count: string
    }
    pageSize: number
    initialLoads?: number
}

@cacheInstances(1)
export class InfiniteList<T> {
    private static server = new BaseServer()

    private count = 0
    private page = 0

    public elements = new Set<T>()
    public refresh = new BehaviorSubject(0)
    public ready = new BehaviorSubject(false)

    constructor(public name: string, private config: InfiniteListConfig) {}

    async init() {
        this.count = await InfiniteList.server.request(this.config.urls.count)

        this.ready.next(true)
        this.update()

        for (let index = 0; index < this.config.initialLoads; index++) {
            this.loadChunk()
        }
    }

    async loadChunk() {
        if (this.elements.size >= this.count) return

        const chunk = await InfiniteList.server.request<T[]>(
            this.config.urls.chunk,
            'GET',
            {},
            {
                page: this.page++,
                pageSize: this.config.pageSize
            }
        )

        for (const element of chunk) {
            this.elements.add(element)
        }

        this.update()
    }

    private update() {
        this.refresh.next(this.refresh.value + 1)
    }

    get data() {
        return Array.from(this.elements.values())
    }
}
