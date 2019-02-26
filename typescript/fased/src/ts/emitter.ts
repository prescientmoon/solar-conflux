type eventKey = string | number;

interface Listener{
    on:eventKey;
    callback:(arg0:any) => void;
}

class EventEmitter{
    listeners:Array<Listener> = [];

    on(on:eventKey,callback:(arg0: any)=>void):EventEmitter{
        this.listeners.push({on,callback});
        return this;
    }
    once(on:eventKey,callback:(arg0: any) => void):void{
        //save lexical scope
        const self:EventEmitter = this;
        const object = {
            on,
            callback:(data) => {
                callback(data);
                self.listeners.splice(self.listeners.indexOf(object),1);
            }
        };

        this.listeners.push(object);
    }
    emit(message:eventKey,data:any):void{
        const event = this.listeners.find(val => val.on == message);
        if (event) event.callback(data);
    }
}

export {EventEmitter};
