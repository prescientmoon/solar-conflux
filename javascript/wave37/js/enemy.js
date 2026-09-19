const G = 1;
class Enemy{
	constructor(config){
		this.updates = [];
		this.lastFrameColiders = [];
		this.configInitial(config.initial);//the initial conditions
		this.configAI(config.AI);//the behavior of the enemy
		this.configPhysics(config.physics);//addPhysics
		this.configCollision(config.collision);//add collision
		this.configShooting(config.battle.shooting);//make it shoot
		this.configBattle(config.battle);//other battling stuff
		this.configRendering(config.rendering);//finally render
		this.extraConfig(config.other);//things that dont really require an update
		this.configOrbitModule(config.orbit);//staying away from the player
		this.configHealing(config.healing);//healing other enemies
		this.configLasers(config.laser);//new attack!!!
		enemies.push(this);
		this.type = "Enemy";
	}
	update(delta){
		for (let i of this.updates){
			this[i](
				delta);
		}
		if (this.hp <= 0){
			bossManager.lag = 1;
			score += this.dieScore;
			enemies.splice(enemies.indexOf(this),1);
			this.explode();
		}
	}
	/*
	initial:{
		RandomPosition:bool,
		***if RandomPosition = false:***
		position:Vector2(number,number),
		***else:***
		//maxvalues
		position:{
			x:number,
			y:number
		},
		rotation:range(0,360),
		speed:Vector2(number,number)
	}
	*/
	configInitial(config){
		//position
		if (config.RandomPosition){
			this.position = new Vec2(Math.random() * config.position.x,Math.random() * config.position.y);
		}
		else{
			this.position = new Vec2(config.position.x,config.position.y);
		}
		this.rotation = config.rotation;
		this.speed = new Vec2(config.speed.x,config.speed.y);
	}
	/*)
	physics:{
		rotation:{
			playerAccuracy:number,
			inertia:number
		},
		thrust:number,
		mass:number,
		collisionCoeficient:number,
	}
	*/
	configPhysics(config){
		this.thrust = config.thrust;
		this.thrustMode = config.thrustMode;
		this.mass = config.mass;
		this.collisionCoeficient = config.collisionCoeficient;
		this.rotationAccuracy = [config.rotation.playerAccuracy,config.rotation.inertia];
		this.updates.push("move");
	}
	move(delta){
		if (this.target == "player"){
			var targetPosition = MainPlayer.position;
			var relativePosition = targetPosition.add(this.position.minus);
		}
		else if(this.target == "playerOrbit"){
			let angle = 0.3;
			var targetPosition = MainPlayer.position;
			var relativePosition = targetPosition.add(this.position.minus);
			let orbitRadius = relativePosition.unit.scale(100).minus;
			orbitRadius = new Vec2(
				Math.cos(angle) * orbitRadius.x - Math.sin(angle) * orbitRadius.y,
				Math.sin(angle) * orbitRadius.x + Math.cos(angle) * orbitRadius.y
			);
			relativePosition = relativePosition.add(orbitRadius);
			if (Math.abs(orbitRadius.magnitude - 100) >= 0.1){
				console.error(`RelativePosition targeting playerOrbit magnitude equals ${orbitRadius.magnitude}`);
			}
		}
		if (this.drawTarget){
			ctx.beginPath();
			ctx.moveTo(this.position.x,this.position.y);
			const end = this.position.add(relativePosition);
			ctx.lineTo(end.x,end.y);
			ctx.strokeStyle = "#00ff00";
			ctx.stroke();
		}
		var velocityToTarget;
		if (this.thrustMode == "linear"){
			velocityToTarget = (relativePosition.unit).scale(this.thrust/this.mass);
		}
		else if (this.thrustMode == "exponential"){
			velocityToTarget = (relativePosition.scale(relativePosition.magnitude)).scale(this.thrust/this.mass)
		}
		else if (this.thrustMode == "directional"){
			velocityToTarget = Vec2.fromAngle((this.rotation-90)*Math.PI/180).scale(this.thrust);
		}
		else{
			console.error(`"${this.thrustMode} is not a valid thrust mode!"`);
		}
		this.speed = this.speed.add(velocityToTarget);
		this.position = this.position.floor();
		this.position = this.position.add(
			this.speed.scale(delta * 60 / 1000)
		);
		if (delta == undefined)
			console.error("Oof");
		const absoluteRelativePosition = targetPosition.add(this.position.minus);
		this.rotation = ((90 + Math.atan2(absoluteRelativePosition.y,absoluteRelativePosition.x)*180/Math.PI)
						* this.rotationAccuracy[0] + this.rotation * this.rotationAccuracy[1])
						/ (this.rotationAccuracy[0] + this.rotationAccuracy[1]);
	}
	/*
	collision:{
		wall:{
			trigger:bool
		},
		enemies:{
			trigger:bool		
		},
		player:{
			trigger:bool,
			hpLost: number,
			damageAmount: number	
		}
	}
	*/
	configCollision(config){
		if (config.enemies.trigger){
			this.enemyCollisionMode = config.enemies.mode;
			this.updates.push("enemyCollision");
			this.checkCollision = true;
		}
		else
			this.checkCollision = false;
		if (config.wall.trigger){
			this.updates.push("wallCollision");
		}
		if (config.player.trigger){
			this.collisionWithPlayerSelfDamage = config.player.hpLost;
			this.collisionWithPlayerDamage = config.player.damageAmount;
			this.updates.push("playerCollision");
		}	
	}
	enemyCollision(){
		for (let i of  enemies){
			if (i != this && 
				Math.abs((i.position.add(this.position.minus)).magnitude) <= 
				(this.size + i.size)/2 && i.checkCollision){
				this.resolveCollisionWithEnemies(i);
				return true;
			}
		}
	}
	playerCollision(){
		if (Math.abs((MainPlayer.position.add(this.position.minus)).magnitude) <= (this.size + MainPlayer.size/2.5)/2){
			MainPlayer.hp -= this.collisionWithPlayerDamage;
			this.hp -= this.collisionWithPlayerSelfDamage;
			MainPlayer.damage(this.collisionWithPlayerDamage * 10);
		}
	}
	wallCollision(){
		for (let i of  walls){
			if (Math.abs(Wall.distToSegment(this.position,i.start,i.end)) <= this.size/2){
				this.resolveCollisionWithWalls(i);
				return true;
			}
		}
	}
	resolveCollisionWithEnemies(enemy){
		if (this.enemyCollisionMode == "dynamic"){
			this.position = this.position.add(this.speed.minus);
			const targetMomentum = (this.momentum.add(enemy.momentum)).scale(1/2);
			this.speed = targetMomentum.scale(1/this.mass);
			enemy.speed = targetMomentum.scale(1/enemy.mass);
		}
		else if (this.enemyCollisionMode == "static"){
			this.position = this.position.add(this.speed.minus);
			this.speed = Vec2.zero();
		}
		else{
			console.error(`"${this.enemyCollisionMode} is not a valid enemy collision mode!"`);
		}
	}
	resolveCollisionWithWalls(wall){
		this.position = this.position.add(this.speed.minus);
		this.speed = this.speed.minus.scale(this.collisionCoeficient);
	}
	/*config rendering:
	rendering: {
		texture: image, 
		size: number
	}
	*/
	configRendering(config){
		this.opacity = config.opacity;
		this.size = config.size;
		this.texture = this.load(config.texture);
		//this.updates.push("render");
	}
	load(url){
		Enemy.getId(this);
		const element = new Image(this.size,this.size);
		element.src = url;
		return element;
	}
	render(){
		//MainCanvas.drawImage(this.texture,this.position.x,this.position.y,this.size,this.size,this.rotation,this.opcaity);
	}
	/*
	shooting: {
		trigger:bool,
		shootMode:"timed"/"randomised",
		shootRate:number,
		bulletAtk:number,
		bulletSpeed:number,
		bulletSize:number,
		shootAngles:[numbers],
		bulletParticles:{
			color:[3-numbers],
			lifeSpan:number,
			count:number,
			scale:number,
			size:number
			randomNess:number
		}
	}
	*/
	configShooting(config){
		this.shootTrigger = config.trigger;
		this.shootMode = config.shootMode;
		this.shootRate = config.shootRate;
		this.bulletAtk = config.bulletAtk;
		this.bulletSpeed = config.bulletSpeed;
		this.shootAngles = config.shootAngles;
		this.shootRate = config.shootRate;
		this.bulletSize = config.bulletSize;
		this.bulletParticleSettings = config.bulletParticles;
		this.wallCol = config.wallCol;
		if (typeof(this.shootTrigger) != "boolean"){
			console.error(`Trigger for shooting must be a boolean. ${typeof(this.shootTrigger)} given`)
		}
		if (this.shootTrigger == true && this.shootAngles.length == 0){
			console.error("Shooing enabled but empty array of angles");
		}
		if (this.shootMode == "timed"){
			this.shootProgress = Math.floor(Math.random() * this.shootRate);
			if (this.shootTrigger){
				this.updates.push("timeShoot");
			}
		}
		else if (this.shootMode == "random"){
			if (this.shootTrigger){
				this.updates.push("randomShoot");
			}
		}
	}
	timeShoot(){
		if (this.shootProgress++ >= this.shootRate){
			this.shootProgress %= this.shootRate;
			for (let i of this.shootAngles){
				this.shoot(i);
			}
		}
	}
	randomShoot(){
		if (Math.floor(Math.random() * this.shootRate) == 0){
			for (let i of this.shootAngles){
				this.shoot(i);
			}
		}
	}
	configBattle(config){
		this.hp = config.hp;
	}
	extraConfig(config){
		this.dieScore = config.dieScore;
		this.color = config.color;
	}
	/*
	orbit:{
		trigger:bool,
		playerEmissive:number,
		enemyEmissive:number,
		playerForceZone:number,
		enemyForceZone:number
	}
	*/
	configOrbitModule(config){
		if (config.trigger){
			this.playerEmissive = config.playerEmissive;
			this.enemyForceZone = config.enemyForceZone;
			this.playerForceZone = config.playerForceZone;
			this.enemyEmissive = config.enemyEmissive;
			this.updates.push("orbit");
		}
	}
	orbit(){
		if (Math.abs((MainPlayer.position.add(this.position.minus)).magnitude) <= this.playerForceZone){		
			const direction = MainPlayer.position.add(this.position.minus).unit;
			const distance = MainPlayer.position.add(this.position.minus).magnitude;
			this.addForce(direction.scale(-this.playerEmissive /distance));
		}

		for (let i of enemies){
			if (i != this && Math.abs((i.position.add(this.position.minus)).magnitude) <= this.enemyForceZone){
				const direction = i.position.add(this.position.minus).unit;
				const distance = i.position.add(this.position.minus).magnitude;
				this.addForce(direction.scale(-this.enemyEmissive /distance));
			}
		}
	}
	configAI(config){
		this.drawTarget = config.drawTarget;
		this.target = config.target;
		if (this.target != "player" && this.target != "playerOrbit"){
			console.error(`${this.target} is not a valid target for enemies`);
		}
	}
	/*
	healing:{
        trigger:false,
        hp:25,
        interval:120,
        count:1,
        render:true,
        color:"#0000ff"
    }
    */
	configHealing(config){
		this.hpToHeal = config.hp;
		this.hpHealInterval = config.interval;
		this.hpHealingCount = config.count;
		this.renderHealing = config.render;
		this.healingColor = config.color;
		this.healTime = Math.floor(Math.random() * this.hpHealInterval);
		this.healTrigger = config.trigger;
	}
	heal(){
		if (!this.healTrigger)
			return 0;
		var toheal = false;
		if (++this.healTime >= this.hpHealInterval){
			toheal = true;
			this.healTime %= this.hpHealInterval;
		}
		const dontHeal = [this];
		for (let i = 0;i < this.hpHealingCount;i++){
			const enemiesLeft = enemies.getArrayWithoutElements(dontHeal);
			if (enemiesLeft.length != 0){
				const other = enemiesLeft.hasMin(this);
				if (toheal){
					other.hp += this.hpToHeal;
				}
				if (this.renderHealing){
					ctx.beginPath();
					ctx.moveTo(this.position.x,this.position.y);
					ctx.lineTo(other.position.x,other.position.y);
					ctx.strokeStyle = this.healingColor;
					ctx.stroke();
				}
				dontHeal.push(other);
			}
		}
	}
	configLasers(config){
		this.laserDirections = config.angles;
		this.laserTime = config.interval + config.frameCount;
		this.laserTimer = Math.floor(Math.random() * this.laserTime);
		this.laserProgressTime = config.frameCount;
		this.laserDamage = config.hp;
		this.lasers = [];
		this.laserOffset = config.offset;
		if (config.trigger){
			this.updates.push("laserShoot");
		}
	}
	laserShoot(){
		if (this.laserTimer++ >= this.laserTime){
			this.laserTimer %= this.laserTime;
			for (let i of this.laserDirections){
				this.addLaser();
			}
		}
		for (let i of this.lasers){
			if (++i.life >= i.limit){
				this.lasers.remove(i);
			}
			else{
				if (Math.abs(Wall.distToSegment(
					MainPlayer.position,this.position,this.position.add( 
					Vec2.fromAngle((this.rotation-90) * Math.PI / 180).scale(3000)) 
				)) <= this.size/this.laserOffset){
					//damaging the player
					MainPlayer.hp -= this.laserDamage/this.laserProgressTime;
					MainPlayer.damage(this.laserDamage/this.laserProgressTime);
				}
			}
		}
	}
	addLaser(){
		this.lasers.push({
			limit:this.laserProgressTime,
			life:0,
			time:0,
			frame:0
		});
	}
	explode(){
		for (var i = 0; i < 100; i++) {
			new Particle(ctx,this.position.x,this.position.y,0,0,1,5,100,0.5,false,this.color);
		}
	}
	shoot(angle){
		new eBullet(this.position.x,this.position.y,
			-Math.sin(-Turtle.radians(angle + this.rotation)) * this.bulletSpeed,
			-Math.cos(-Turtle.radians(angle + this.rotation)) * this.bulletSpeed,
			this.bulletSize,this.bulletAtk,this.bulletParticleSettings.scale,
			this.bulletParticleSettings.size,this.bulletParticleSettings.lifeSpan,
			this.bulletParticleSettings.randomNess,this.bulletParticleSettings.color,
			this.bulletParticleSettings.count);
	}
	addForce(vector){
		this.speed = this.speed.add(vector.scale(1/this.mass));
	}
	get momentum(){
		return this.speed.scale(this.mass);
	}
	static getId(enemy){
		enemy.id = Enemy.curentId++; 
		return enemy.id;
	}
}
Enemy.curentId = 0;

Array.prototype.inArray = function(element){
	for (let i of this){
		if (element == i) return true;
	}
	return false;
}

Array.prototype.getArrayWithoutElements = function(obj){
 	var res = [];
 	for (let i of this){
 		if (obj.indexOf(i) == -1){
 			res.push(i);
 		}
 	}
 	return res;
}
Array.prototype.hasMin = function(exception) {
    return this.reduce(function(prev, curr){ 
        return (prev.position.dt(exception.position) < curr.position.dt(exception.position)) ? prev : curr; 
    });
}

Array.prototype.remove = function(element){
	this.splice(this.indexOf(element,1));
}