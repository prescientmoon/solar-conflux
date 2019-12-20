class Player{
	constructor(ctx,x,y,size = 50){
		this.position = new Vec2(x,y);
		this.speed = new Vec2(0,0);
		this.rotation = 0;
		this.size = size;
		this.radius = this.size/5;
		this.ctx = ctx;
		this.particlePerFrame = 10;
		this.isThrusting = false;
		this.thrust = 0.1;
		this.microForce = 0.00005;
		this.friction = 0.98;
		this.mass = 1;
		this.rotationSpeed = 0;
		this.RotationThrust = 0.35;
		this.phoneThrust = 1;
		this.rotationFriction = 0.98;
		this.colided = false;
		this.hp = 100;
		this.mhp = 100;
		this.regenerate = 0.01;
		this.bulletTexture = null;
		this.bulletAttach = null; 
		this.loadTime = 512;
		this.progress = 0;
		this.shock = 25;
		this.healValue = 10;
		this.rotationAccel = 0;
		this.bounceFactor = 0.2;
		this.type = "Player";
		this.shootSfx = new Audio("audio/shoot.wav");
		this.healSfx = new Audio("audio/blessing.ogg")
		this.damageTime = 60;
		this.damageTimer = 0;
		this.damaged = false;
		this.shootAngles = [0];
	}
	update(delta){
		this.progress++;
		this.hp += this.regenerate;
		if (this.mhp < this.hp){
			this.mhp = this.hp;
		}
		this.position = this.position.add(
			this.speed.scale(delta * 60 / 1000)
		);
		this.rotationSpeed += this.rotationAccel;
		this.rotation += this.rotationSpeed * delta * 60 / 1000;
		this.rotation %= 360;

		if (this.checkColisionWithWall()){
			this.resolveColision();
			this.colided = true;
		}
		else {
			this.colided  = false;
		}

		if (this.isThrusting){
			this.addThrust();
			if (this.progress > this.loadTime){
				var color = [256,0,0];
				if (abilityTuroial && tutorial){
					abilityTuroial = false;
					MainLoop.stop();
					dialog(`AFG:Initialising extra-ability protocol...         <br/>
						Extra-ability succesfully installed.         <br/>
						You can use the special ability if you have a red trail     <br/>
					    You can deal damage to all enemies with Q or heal with E.         <br/>
					    Good luck!`,50,true,() => {
					    	MainLoop.start();
					    	$("#dialog").fadeOut(300);
					    });
				}
			}
			else{
				var color = [128,0,256];
			}
			for (let i = 0;i < this.particlePerFrame;i++){
				new Particle(this.ctx,this.position.x + Math.sin(-Turtle.radians(this.rotation)) * this.radius,
					this.position.y + Math.cos(-Turtle.radians(this.rotation))*this.radius,
					-Math.sin(-Turtle.radians(this.rotation)) * -1,
					-Math.cos(-Turtle.radians(this.rotation)) * -1,
					1,3,30,2,true,color);
				particles[particles.length - 1].triggerCollision = true;
			}
		}

		this.applyFriction();

		if (this.damaged && ++this.damageTimer >= this.damageTime){
			MainCanvas.cameraAmount = 0;
			this.damaged = false;
		}
	}
	addThrust(){
		this.speed.x += -Math.sin(-Turtle.radians(this.rotation)) * this.thrust;
		this.speed.y += -Math.cos(-Turtle.radians(this.rotation)) * this.thrust;
	}
	applyFriction(){
		this.speed = this.speed.scale(this.friction);
		this.rotationSpeed *= this.rotationFriction;
	}
	microThrust(){
		if (!this.colided){
			this.speed.x += -Math.sin(-Turtle.radians(this.rotation)) * this.microForce;
			this.speed.y += -Math.cos(-Turtle.radians(this.rotation)) * this.microForce;
		}
	}
	checkColisionWithWall(){
		for (let i of  walls){
			if (Math.abs(Wall.distToSegment(this.position,i.start,i.end)) <= this.radius){
				return true;
			}
		}
		return false;
	}
	resolveColision(){
		this.position = this.position.add(this.speed.minus);
		this.speed = this.speed.minus.scale(this.bounceFactor);
	}
	shoot(){
		this.shootSfx.currentTime = 0;
		this.shootSfx.play();
		for (let i of this.shootAngles){
		new Bullet(this.position.x,this.position.y,
			-Math.sin(-Turtle.radians(this.rotation + i)) * this.thrust * 50,
			-Math.cos(-Turtle.radians(this.rotation + i)) * this.thrust * 50);
		}
	}
	bomb(){
		if (this.progress >= this.loadTime){
			this.loadTime *= 2;
			this.progress = 0;
			for (let i of enemies){
				i.hp -= this.shock;
			}
			this.explode();
		}
	}
	explode(){
		for (var i = 0; i < 100; i++) {
			new Particle(ctx,this.position.x,this.position.y,0,0,1,5,300,1,false,[Math.random() * 256,Math.random() * 256,Math.random() * 256]);
		}
	}
	explode2(){
		for (var i = 0; i < 50; i++) {
			new Particle(ctx,this.position.x,this.position.y,0,0,1,5,300,1,false,[256,128,0]);
		}
	}
	heal(){
		if (this.progress >= this.loadTime){
			this.healSfx.currentTime = 0;
			this.healSfx.play();
			this.loadTime *= 2;
			this.progress = 0;
			for (let i of enemies){
				this.hp += this.healValue;
			}
			this.explode2();
		}
	}
	addForce(vector){
		this.speed = this.speed.add(vector.scale(1/this.mass));
	}
	damage(value=60){
		this.damaged = true;
		this.damageTimer = 0;
		this.damageTime = value;
		MainCanvas.cameraAmount = 10;
	}
}






