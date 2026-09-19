class Turtle{
	constructor(ctx){
		this.ctx = ctx;
		this.bg = 0;
		this.cameraAmount = 0;
		this.camera = [0,0];
	}
	cameraMove(sign){
		if (sign == 1){
			this.camera = [(Math.random() - 0.5) * this.cameraAmount,
				(Math.random() - 0.5) * this.cameraAmount];
			this.ctx.translate(this.camera[0],this.camera[1]);
		}
		else
			this.ctx.translate(-this.camera[0],-this.camera[1]);
	}
	drawRect(x,y,w,h,angle){
		Turtle.drawRect(this.ctx,x,y,w,h,angle);
		return Turtle;
	}
	drawImage(img,x,y,w,h,angle,opacity=1){
		Turtle.drawImage(this.ctx,img,x,y,w,h,angle,opacity);
		return Turtle;
	}
	clearCtx(){
		Turtle.clearCtx(this.ctx);
		return Turtle;
	}
	static clearCtx(ctx){
		const grayscale = (MainPlayer.mhp - MainPlayer.hp) * 256/MainPlayer.mhp;
		const bg = `rgba(${grayscale},${grayscale},${grayscale},0.7)`
		ctx.fillStyle = bg;
		const temp = $("#canvas").width();
		$("#canvas").attr("width","0px");
		$("#canvas").attr("width",`${temp}px`);
		$("#container").css("background-color",bg)
		ctx.fillStyle = `rgba(256,0,0,${0.2 + Math.floor(Math.random()/3)})`;
		ctx.beginPath();
		ctx.arc(canvas.width / 2,canvas.height / 2,200,0,MainSpawner.time * Math.PI * 2 / MainSpawner.maxTime);
		ctx.lineTo(canvas.width / 2,canvas.height / 2);
		ctx.fill();
		this.bg = grayscale;
	}
	static drawRect(ctx,x,y,w,h,angle){
		ctx.save();
		ctx.translate(x,y);
		ctx.rotate(Turtle.radians(angle));
		Turtle.FillCenteredRect(ctx,w,h);
		ctx.restore();
		return Turtle;
	}
	static drawImage(ctx,img,x,y,w,h,angle,opacity=1){
		ctx.save();
		ctx.translate(Math.floor(x),Math.floor(y));
		ctx.rotate(Turtle.radians(angle));
		Turtle.DrawCenteredImage(ctx,img,Math.floor(w),Math.floor(h));
		ctx.restore();
		ctx.globalAlpha = 1;
		return Turtle;
	}
	static FillCenteredRect(ctx,w,h){
		ctx.fillRect(-w/2,-h/2,w,h);
	}static DrawCenteredImage(ctx,img,w,h){
		if (img == undefined)
			return 0;
		ctx.drawImage(img,-w/2,-h/2,w,h);
	}
	static radians(angle){
		return angle * Math.PI / 180;
	}
}