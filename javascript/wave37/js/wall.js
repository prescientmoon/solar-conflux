class Wall{
	constructor(ctx,x1,y1,x2,y2,color = "#8833ff"){
		this.x = [x1,x2];
		this.y = [y1,y2];

		this.ctx = ctx;
		this.color = color;

		walls.push(this);
	}
	update(){
	}
	show(){
		const temp = this.ctx.strokeStyle;
		this.ctx.strokeStyle  = this.color;
		this.ctx.beginPath();
		this.ctx.moveTo(this.start.x,this.start.y);
		this.ctx.lineTo(this.end.x,this.end.y);
		this.ctx.stroke();
		this.ctx.strokeStyle = temp;
	}
	get start(){
		return new Vec2(w * this.x[0]/100,h * this.y[0]/100);
	}
	get end(){
		return new Vec2(w * this.x[1]/100,h * this.y[1]/100);
	}
	get h(){
		return parseFloat($("#canvas").attr("height"));
	}
	get w(){
		return parseFloat($("#canvas").attr("width"));
	}
	static distToSegmentSquared(p, v, w) {
	  var l2 = Vec2.distance(v, w);
	  if (l2 == 0) return Vec2.distance(p, v);
	  var t = ((p.x - v.x) * (w.x - v.x) + (p.y - v.y) * (w.y - v.y)) / l2;
	  t = Math.max(0, Math.min(1, t));
	  return Vec2.distance(p, { x: v.x + t * (w.x - v.x),
	                    y: v.y + t * (w.y - v.y) });
	}
	static distToSegment(p, v, w){
		return Math.sqrt(Wall.distToSegmentSquared(p, v, w));
	}
}

function sqr(x) { return x * x }