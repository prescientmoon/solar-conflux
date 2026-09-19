class Bullet{
	constructor(x,y,sx,sy,size=5){
		this.position = new Vec2(x,y);
		this.speed = new Vec2(sx,sy).scale(Bullet.speed);
		//this.texture = document.getElementById('texture3');
		this.atk = Bullet.atk;
		this.size = Bullet.size;
		this.pps = 2;
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

		this.show();
		for (var i = 0; i < this.pps; i++) {
			new Particle(ctx,this.position.x,this.position.y,-this.speed.x,-this.speed.y,1,3,50,1,false,[55,256,55]);
		}
	}
	show(){
		MainCanvas.drawImage(this.texture,this.position.x,this.position.y,this.size,this.size,0);
	}
	checkColisionWithWall(){
		if (this.position.x < 0 || this.position.y < 0 || this.position.x > w || this.position.y > h)
			return true;
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
		for (let i of  enemies){
			if (i != this && Math.abs((i.position.add(this.position.minus)).magnitude) <= (this.size + i.size)/2){
				i.hp -= this.atk;
				bullets.splice(bullets.indexOf(this),1);
			}
		}
	}
}

Bullet.atk = 25;
Bullet.size = 5;
Bullet.speed = 1;