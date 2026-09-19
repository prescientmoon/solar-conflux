class eBullet{
	constructor(/*initial*/x,y,sx,sy,size,/*battle*/atk
		/*particle system*/,scale,psize,life,random,color,count){
		this.position = new Vec2(x,y);
		this.speed = new Vec2(sx,sy);
		//this.texture = document.getElementById('texture3');
		this.atk = atk;
		this.size = size;
		this.pps = count;
		this.scale = scale;
		this.psize = psize;
		this.life = life;
		this.random = random;
		this.flare = false;
		this.color = color;
		this.rotation = 0;
		bullets.push(this);

		//type for drawing
		this.type = "Bullet";
	}
	update(delta){
		this.position = this.position.floor();
		this.position = this.position.add(
			this.speed.scale(delta * 60 / 1000)
		);

		if (this.checkColisionWithWall()){
			this.resolveColision();
			this.colided = true;
		}
		else {
			this.colided  = false;
		}
		this.checkColisionWithEntities();

		for (var i = 0; i < this.pps; i++) {
			new Particle(ctx,this.position.x,this.position.y,-this.speed.x,-this.speed.y,this.scale,this.psize,this.life,this.random,this.flare,this.color);
		}
	}
	show(){
		MainCanvas.drawImage(this.texture,Math.floor(this.position.x),
			Math.floor(this.position.y),
			Math.floor(this.size),Math.floor(this.size),0);
	}
	checkColisionWithWall(){
		for (let i of  walls){
			if (Math.abs(Wall.distToSegment(this.position,i.start,i.end)) <= this.size){
				return true;
			}
		}
		return false;
	}
	resolveColision(){
		bullets.splice(bullets.indexOf(this),1);
	}
	checkColisionWithEntities(){
		for (let i of  [MainPlayer]){
			if (i != this && Math.abs((i.position.add(this.position.minus)).magnitude) <= (this.size + i.radius)/2){
				i.hp -= this.atk;
				bullets.splice(bullets.indexOf(this),1);
				i.damage(this.atk);
			}
		}
	}
}