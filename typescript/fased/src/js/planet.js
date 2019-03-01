class Planet {
    constructor(radius, mass, position, locked = false, image) {
        this.radius = radius;
        this.mass = mass;
        this.position = position;
        this.locked = locked;
        this.image = image;
        this.enabled = true;
        this.speed = [0, 0];
    }
}
export { Planet };
