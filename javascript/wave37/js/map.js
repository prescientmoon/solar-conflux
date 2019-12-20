{
	rendering: {
		texture: imageURL, 
		size: number
	},
	AI:{
		target:"player"/"playerOrbit"
	}
	initial:{
		RandomPosition:bool,
		***if RandomPosition = false:***
		position:Vector2(number,number),
		***else:***
		//maxvalues(example of vec2)
		position:{
			x:number,
			y:number
		},
		rotation:range(0,360),
		speed:Vector2(number,number)
	},
	physics:{
		rotation:{
			playerAccuracy:number,
			inertia:number
		},
		thrust:number,
		thrustMode:"exponential"/"linear",
		mass:number,
		collisionCoeficient:number,
	}
	collision:{
		wall:{
			trigger:bool
		},
		enemies:{
			trigger:bool,
			mode:"dynamic"/"static"		
		},
		player:{
			trigger:bool,
			loseHP:bool,
			hpLost: number,
			damage: bool:
			damageAmount: number	
		}
	},
	battle:{
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
		},
		hp:number
	},
	other:{
		dieScore:number,
		spawnScore:number>dieScore,
		color:[3-numbers]
	},
	orbit:{
		trigger:bool,
		playerEmissive:number,
		enemyEmissive:number,
		playerForceZone:number,
		enemyForceZone:number
	}
}



