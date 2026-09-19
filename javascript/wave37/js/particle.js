class Particle{
	constructor(ctx,x,y,sx,sy,scale=1,size=3,lifeSpan=200,randomMultiplayer=2,flare=true,color=[128,0,256]){
		this.position = new Vec2(x,y);
		this.speed = new Vec2(sx/scale,sy/scale);
		this.size = size;
		this.color = color;
		this.randomMultiplayer = randomMultiplayer;
		this.lifeSpan = lifeSpan;
		this.originalSpan = lifeSpan;
		this.age = 0;
		this.mass = 1;
		this.colisioTolerance = 1;
		this.flareColor = [256,256,256];
		this.flare = flare;
		this.triggerCollision = false;
		particles.push(this);
	}
	update(delta){
		this.age++;
		this.position = this.position.floor();
		this.position = this.position.add(
			this.speed.scale(delta * 60 / 1000)
		);
		this.speed.x += (Math.random() - 0.5)/this.randomMultiplayer;
		this.speed.y += (Math.random() - 0.5)/this.randomMultiplayer;
		//this.position.x += (Math.random() - 0.5)/this.randomMultiplayer;
		//this.position.y += (Math.random() - 0.5)/this.randomMultiplayer;
		//this.show();
		if (this.age >= this.lifeSpan){
			particles.splice(particles.indexOf(this),1);
		}
		if (this.triggerCollision && this.checkColisionWithWall()){
			this.resolveColision();
		}
	}
	show(){
		const dif = this.lifeSpan - this.age;
		const temp = ctx.fillStyle;
		ctx.fillStyle = `rgba(${this.color[0]},${this.color[1]},${this.color[2]},
		${Math.floor(10 * dif/this.lifeSpan)/10}`;
		ctx.fillRect(Math.floor(this.position.x),
			Math.floor(this.position.y),
			Math.floor(this.size),
			Math.floor(this.size));
		ctx.fillStyle = temp;
	}
	resolveColision(){
		if (this.flare){
			this.color = this.flareColor;
			this.speed.x = 0;
			this.speed.y = 0;
			this.size = 5;
			this.lifeSpan = this.originalSpan * 1.3;
			this.randomMultiplayer = 50;
			MainPlayer.microThrust();
		}
	}
	checkColisionWithWall(){
		for (let i of walls){
			if (Math.abs(Wall.distToSegment(this.position,i.start,i.end)) <= this.colisioTolerance){
				return true;
			}
			//else {console.log(Math.abs(Wall.distToSegment(i.start,i.end,this.position)))}
		}
		return false;
	}
	addForce(vector){
		this.speed = this.speed.add(vector.scale(1/this.mass));
	}
}





