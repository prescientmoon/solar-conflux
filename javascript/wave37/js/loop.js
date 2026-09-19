//textures for drwing images
const playerImage = new Image(30,30);
playerImage.src = "textures/player.png";
const bulletImage = new Image(10,10);
bulletImage.src = "textures/bullet.png";
const laserTexture = new Image(10,10);
laserTexture.src = "textures/laser.png";
const laserFrameDrop = 3;
function texture(obj){
	const type = obj.type;
	let img = new Image(30,30);
	if (type == "Bullet")
		return bulletImage;
	else if (type == "Player")
		return playerImage;
	else if (type == "Enemy")
		return obj.texture;
	console.log(img,obj);
}

//gets everything to update
function getUpdatable(){
	return [...enemies,bossManager,...particles,...bullets,MainPlayer,
	...walls,MainSpawner];
}

function getImgDrawable(){
	return [...bullets,MainPlayer,...enemies];
}

//update the scene
function update(delta){
	const updatable = getUpdatable();
	for (let i of updatable){
		i.update(delta);
	}
	clearParticles();
}

//drawws the scene
function draw(){
	MainCanvas.cameraMove(1);
	//clear the canvas
	MainCanvas.clearCtx();
	

	//get the enemy lasers
	for (let i of enemies){
		for (let j of i.lasers){
			if (++j.time%laserFrameDrop == 0){
				j.frame++;
				j.frame %= 9;
			}
			const resultantVector = 
				i.position.add(Vec2.fromAngle(i.rotation).scale(1500));
			ctx.save();
			ctx.translate(i.position.x,i.position.y);
			ctx.rotate(i.rotation * Math.PI/180);
			ctx.drawImage(laserTexture,
				Math.floor(j.frame * 2286/9),0,Math.floor(2286/9),1200,
				-i.size/(2*i.laserOffset),-3000,i.size/i.laserOffset,3000);
			ctx.restore();
		}
	}

	//get drawable elements
	const drawable = getImgDrawable();

	for (let i of particles){
		const dif = i.lifeSpan - i.age;
		ctx.fillStyle = `rgba(${i.color[0]},${i.color[1]},${i.color[2]},
		${Math.floor(10 * dif/i.lifeSpan)/10}`;
		ctx.fillRect(Math.floor(i.position.x),
			Math.floor(i.position.y),
			Math.floor(i.size),
			Math.floor(i.size));
	}

	for (let i of drawable){
		MainCanvas.drawImage(texture(i),
			i.position.x,
			i.position.y,
			i.size,i.size,i.rotation);
	}

	ctx.strokeStyle  = "#8833ff";
	ctx.beginPath();
	for (let i of walls){
		ctx.moveTo(i.start.x,i.start.y);
		ctx.lineTo(i.end.x,i.end.y);
	}
	ctx.stroke();
	for (let i of enemies){
		i.heal();
	}
	MainCanvas.cameraMove(-1);
}

//runs at the end of every frame
function end(){
	clearParticles();
	if (MainPlayer.hp <= 0 && !paused){
		die();
		MainLoop.stop();
	}
}

function clearParticles(){
	while (particles.length >= performancLimit){
		particles.splice(Math.floor(Math.random() * particles.length));
	}
}

MainLoop.setUpdate(update).setDraw(draw).setEnd(end);