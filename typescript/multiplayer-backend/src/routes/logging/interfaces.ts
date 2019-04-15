export interface log{
    title:string;
    body:string;
    time:Date;
    elapsed?:number;
}
export interface shortConsole{
    logs:log[];
    log:(name:string,body:string) => any;
}