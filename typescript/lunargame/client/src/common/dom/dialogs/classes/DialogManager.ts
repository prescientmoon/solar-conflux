import { Singleton } from '@eix/utils'
import { BehaviorSubject } from 'rxjs'

export interface DialogAction {
    name: string
    variant: 'standard' | 'contained' | 'outlined'
    callback: Function
}

export interface Dialog {
    title: string
    message: string
    actions: DialogAction[]
    onClose: (event: unknown) => void
}

@Singleton
export class DialogManager {
    public active = new BehaviorSubject<Dialog | null>(null)
    public queue: Dialog[] = []

    public add(dialog: Dialog) {
        if (this.active.value !== null) {
            this.queue.push(dialog)
        } else {
            this.activate(dialog)
        }
    }

    private activate(dialog: Dialog) {
        this.active.next({
            ...dialog,
            onClose: (event: unknown) => {
                dialog.onClose(event)

                if (this.queue.length) {
                    const newDialog = this.queue.shift()
                    this.activate(newDialog)
                } else {
                    this.active.next(null)
                }
            }
        })
    }
}
