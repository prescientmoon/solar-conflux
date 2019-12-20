var char;
var notStopped = false;
var dialoged = {
	a:false,
	get absolute(){
		return this.a;
	},
	set absolute(value){
		this.a = value;
		if (value == false){
			this.relative = false;
			console.log("Attempting to call callback!");
			dialog.callback().then((result) => {
				console.log(`Async call ended with code ${result}`);
			});
		}
	},
	relative:false
};
class Spawner{
	constructor(){
		this.wave = 0;
		this.time = 999;
		this.maxTime = 1000;
		$("#wave").fadeOut(1000);
		//score += 20000;
	}
	update(){
		if (this.wave == 37)
			return;
		this.time++
		if (this.time >= this.maxTime || enemies.length == 0){
			this.nextWave();
		}
	}
	nextWave(){
		this.wave++;
		$("#wave").text(`Wave: ${this.wave}`);
		this.time = 0;
		if (this.wave == 10){
			MainPlayer.shootAngles.push(-15);
			MainPlayer.shootAngles.push(15);
			MainLoop.stop();
			dialog(`AFG:Installing upgrades...       <br/>
				Rebooting ship...`,50,true,() => {
					dialog(`I managed to install the upgrade to yoru ship.       <br/>
						Now you can shoot 3 bullets at a time.       <br/>
						Goon luck!`,50,true,() => {
							$("#dialog").fadeOut(300);
							MainLoop.start();
					});
			});
		}
		else if (this.wave == 20){
			MainPlayer.shootAngles = [0,90,180,270];
			Bullet.size = 10;
			Bullet.atk = 100;
			Bullet.speed = 5;
			MainLoop.stop();
			dialog(`AFG:Installing upgrades...       <br/>
				Rebooting ship...`,50,true,() => {
					dialog(`I managed to install the upgrade to yoru ship.       <br/>
						Now your ship is optimised for a large amount of enemies.       <br/>
						Goon luck!`,50,true,() => {
							$("#dialog").fadeOut(300);
							MainLoop.start();
					});
			});
		}
		else if (this.wave == 37){
			MainLoop.stop();
			dialog(`Thanks for helping me with my project!!!
				<br/>Please enter that spiny thing!!!
				<br/>I promise you nothing bad would happen!!!
				`,20,true,() => {
					$("#canvas").css("background-color","#000000");
					$("body").css("background-image","url('textures/moon.png')");
					walls.splice(0,2);
					MainPlayer.friction = 0.95;
					$("#dialog").fadeOut(300);
					MainLoop.start();
				});
			enemies = [];
			return 0;
		}
		MainPlayer.mhp += this.wave;
		MainPlayer.hp += this.wave;
		MainPlayer.regenerate += 0.0005;
		let remaining = score;

		for (let i of packs){
			const spawnCount = Math.floor(remaining / i.spawnScore);
			//console.log(spawnCount);
			remaining %= i.spawnScore;
			for (let j = 0;j < spawnCount;j++){
				this.spawn(i.components);
			}
		}

		$("#wave").show(100);
		$("#wave").fadeOut(1000);
	}
	spawn(blueprint){
		if (this.wave == 37)
			return 0;
		const tospawn = blueprint[Math.floor(Math.random() * blueprint.length)];
		for (let k of tospawn){
			//console.log(k);
			for (let l = 0;l < k.count;l++){
				//console.log("Spawnig!!!");
				createEnemy(k.blueprint);
				//console.log(l);
			}
		}
	}
}

function dialog(mes,time,first=true,cb=undefined){
	if (first){
		console.log("Initialising new dialog!!!");
		dialog.call = cb;
		$("#dialog-content").html("");
		$("#dialog-footer").hide();
		$("#dialog").show();
		char = 0;
		dialoged.absolute = true;
		dialoged.relative = true;
		return dialog(mes,time,false);
	}
	if (++char < mes.length && dialoged.relative){
		let string = "";
		for (let i = 0;i < char;i++)
			string += mes[i];
		if (string == mes)
			dialoged.relative = false;
		$("#dialog-content").html(string);
		setTimeout(() => {
			dialog(mes,time,false);
		},time);
		return 0;
	}
	$("#dialog-content").html(mes);
	$("#dialog-footer").show();
}
dialog.callback = async function(){
	return dialog.call();
}
dialog.fadeMode = "Instant";
dialog.hide = function(){
	$("#dialog").fadeOut((dialog.fadeMode == "Instant")?10:1000);
}
function W37start(){
	bossManager.w37();
}
