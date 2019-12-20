class boss{
	constructor(){
		this.blurModifier = 100000;
		this.brightnessModifier = 50000;
		this.ppi = 1;
		this.start = false;
		this.gravity = 0.03;
		this.texture = document.getElementById('hole');
		this.color = [256,256,256];
		this.position = new Vec2(canvas.width/2,canvas.height/2);
		this.blurLimit = 700;
		//lag for enemy destroying effect
		this.lag = 0;
		this.lagMultiplier = 0.9;
		this.lagTime = 75;
	}
	w37(){
		enemies = [];
		this.start = true;
		performancLimit = 50000;
		$("#hole").fadeIn(100);
		$("#hole").css({
			"top":`${canvas.height/2 - 150}px`,
			"left":`${canvas.width/2 - 150}px`,
			"width":"300px",
			"height":"300px"
		});
	}
	update(delta){
		if (this.start){
			for (var i = 0; i < this.ppi; i++){
				new Particle(ctx,this.position.y,this.position.x,0,0,1,0.5,100,0.5,true,this.color);
			}
			for (var i = 0; i < this.ppi; i++){
				new Particle(ctx,0,0,0,0,1,2,100,0.5,false,this.color);
			}
			for (var i = 0; i < this.ppi; i++){
				new Particle(ctx,0,canvas.height,0,0,1,2,100,0.5,false,this.color);
			}
			for (var i = 0; i < this.ppi; i++){
				new Particle(ctx,canvas.width,0,0,0,1,2,100,0.5,false,this.color);
			}
			for (var i = 0; i < this.ppi; i++){
				new Particle(ctx,canvas.width,canvas.height,0,0,1,2,100,0.5,false,this.color);
			}
			ctx.beginPath();
			ctx.fillStyle = "rgba(0,0,256,0.1)";
			ctx.moveTo(this.getMiddle.x,this.getMiddle.y);
			ctx.arc(this.getMiddle.x,this.getMiddle.y,this.blurLimit,0,2*Math.PI);
			ctx.fill();
			//ctx.beginPath();
			//ctx.fillStyle = "rgba(256,256,256,0.3)";
			//ctx.arc(this.getMiddle.x,this.getMiddle.y,300,0,2*Math.PI);
			//ctx.fill();
			MainPlayer.addForce(MainPlayer.position.add(this.getMiddle.minus).unit.minus.scale(this.gravity));
			if (1/this.getValue <= this.blurLimit){
				$("#container").css("filter",`blur(${this.blurModifier * Math.pow(this.getValue,2)}px)`);
				$("#container-2").css("filter",`brightness(${this.brightnessModifier * this.getValue}%)`);
				//$("canvas").css("filter",`brightness(200%)`);
			}
			else {
				$("canvas").css("filter",`blur(0px)`);
				$("body").css("filter",`brightness(100%)`);
			}
			for (let i of particles){
				i.addForce(i.position.add(this.getMiddle.minus).unit.minus.scale(this.gravity));
			}
		}
		//$("#hue-rotation").css("filter",`contrast(${this.angle}%)`)
	}
	getFrameLength(){
		//1000/60 ....................... blurLimit
		//1000/x  ....................... curent
		//
		//1000/x = blurLimit x 1000 / (60 * curent)
		//
		if (1/this.getValue <= this.blurLimit && this.start){
			return 1/this.getValue * 1000 / (60 * this.blurLimit);
			//return this.blurLimit * 1000 / (60 * 1 / this.getValue);
		}
		let res = 0;
		if (this.lag > 0){
			res = this.lagTime/this.lag;
			this.lag *= 2;
		}
		return res;
	}
	get getMiddle(){
		return new Vec2(canvas.width/2,canvas.height/2);
	}
	get getValue(){
		return 1 / MainPlayer.position.add(this.getMiddle.minus).magnitude;
	}
	get angle(){
		if (this.lag <= Math.pow(2,this.lagTime - 1))
			return  200;
		return 100;
	}
}
