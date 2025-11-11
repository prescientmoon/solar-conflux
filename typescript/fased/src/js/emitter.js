class EventEmitter {
    constructor() {
        this.listeners = [];
    }
    on(on, callback) {
        this.listeners.push({ on, callback });
        return this;
    }
    once(on, callback) {
        const self = this;
        const object = {
            on,
            callback: (data) => {
                callback(data);
                self.listeners.splice(self.listeners.indexOf(object), 1);
            }
        };
        this.listeners.push(object);
    }
    emit(message, data) {
        const event = this.listeners.find(val => val.on == message);
        if (event)
            event.callback(data);
    }
}
export { EventEmitter };
