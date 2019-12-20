var bp = testConfig4 = {
	initial:{
		RandomPosition:true,
		position:{
			x:800,
			y:500
		},
		rotation:0,
		speed:{
			x:0,
			y:0
		}
	},
	AI:{
		target:"player",
		drawTarget:false
	},
	rendering: {
		texture: "",
		size: 30,
		opacity:1
	},
	physics:{
		rotation:{
			playerAccuracy:1,
			inertia:0
		},
		thrust:0.05,
		thrustMode:"linear",
		mass:1,
		collisionCoeficient:0.075
	},
	collision:{
		enemies:{
			trigger:true,
			mode:"static"
		},
		wall:{
			trigger:true
		},
		player:{
			trigger:true,
			hpLost: 25,
			damageAmount: 5
		}
	},
	battle:{
		hp:25,
		shooting: {
			trigger:false,
			shootMode:"timed",
			shootRate:300,
			bulletAtk:25,
			bulletSpeed:7,
			bulletSize:5,
			shootAngles:[0],
			bulletParticles:{
				color:[256,0,0],
				lifeSpan:30,
				count:3,
				scale:1,
				size:3,
				randomNess:1
			},
			wallCollision:true
		}
	},
	other:{
		dieScore:5,
		color:[0,0,256]
	},
	orbit:{
		trigger:false,
		playerEmissive:10,
		playerForceZone:500,
		enemyEmissive:5,
		enemyForceZone:100
	},
	healing:{
		trigger:false,
		hp:25,
		interval:120,
		count:1,
		render:true,
		color:"#0000ff"
	},
	laser:{
		trigger:false,
		frameCount:120,
		interval:360,
		hp:50,
		angles:[0],
		offset:2
	}
}
function createEnemy(data,times=1){
	var config = data;
	var blueprint = objadd(data,bp);
	for (let i = 0;i < times;i++){
		new Enemy(blueprint);
	}
}
function objadd(data,object){
	const clone = jsonCopy(object);
	for (let i in data){
		if (typeof(data[i]) == "object"){
			clone[i] = objadd(data[i],clone[i]);
		}
		else {
			clone[i] = data[i];
		}
	}
	return clone;
}
function jsonCopy(object){
	return JSON.parse(JSON.stringify(object));
}
