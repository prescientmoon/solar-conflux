const precision = 0.00000001;
class Vec2{
	constructor(x,y){
		this.position = [x,y];
	}
	add(other){
		return new Vec2(this.x + other.x,this.y + other.y);
	}
	minus(){
		return new Vec2(-this.x,-this.y);
	}
	scale(scalar){
		return Vec2.scale(scalar,this);
	}
	unit(){
		return this.scale(1/this.magnitude());
	}
	magnitude(){
		return Math.sqrt(Math.pow(this.x,2) + Math.pow(this.y,2));
	}
	dt(other){
		return (other.add(this.minus)).magnitude;
	}
	floor(){
		return new Vec2(Math.floor(this.x),Math.floor(this.y));
	}
	get x(){
		return this.position[0];
	}
	get y(){
		return this.position[1];
	}
	get minus(){
		return new Vec2(-this.x,-this.y);
	}
	set x(value){
		this.position[0] = value;
	}
	set y(value){
		this.position[1] = value;
	}
	get unit(){
		return this.scale(1/this.magnitude);
	}
	get magnitude(){
		return Math.sqrt(Math.pow(this.x,2) + Math.pow(this.y,2));
	}
	static add(vec1,vec2){
		return new Vec2(vec1.x + vec2.x,vec1.y + vec2.y);
	}
	static minus(vec){
		return new Vec2(-vec.x,-vec.y);
	}
	static Vec2(x,y){
		return new Vec2(x,y);
	}
	static scale(scalar,vector){
		return new Vec2(vector.x * scalar,vector.y * scalar);
	}
	static distance(vec1,vec2){
		return sqr(vec1.x - vec2.x) + sqr(vec1.y - vec2.y);
	}
	static unit(vec){
		return vec.scale1/(Vec2.magnitude(vec));
	}
	static magnitude(vec){
		return Math.sqrt(Math.pow(vec.x,2) + Math.pow(vec.y,2));
	}
	static zero(){
		return new Vec2(0,0);
	}
	static fromAngle(angle){
		return new Vec2(Math.cos(angle),Math.sin(angle));
	}
}




