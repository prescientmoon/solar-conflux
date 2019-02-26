class Planet {
    constructor(radius, mass, position, locked = false) {
        this.radius = radius;
        this.mass = mass;
        this.position = position;
        this.locked = locked;
        this.enabled = true;
        this.speed = [0, 0];
    }
}
export { Planet };
