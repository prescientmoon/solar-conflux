class Planet {
    enabled: boolean = true;
    speed: [number, number] = [0, 0];
    constructor(
        public radius: number,
        public mass: number,
        public position: [number, number] | Array<number>,
        public locked:boolean = false) { }
}

export { Planet };