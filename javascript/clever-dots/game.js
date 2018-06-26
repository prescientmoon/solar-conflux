function game(){
	this.size = [500,500];
	
	this.clear = function(){
		setTimeout(function(){
			var c = document.getElementById("can");
			var ctx = c.getContext("2d");
			ctx.fillStyle="#000000";
			ctx.fillRect(0,0,1000,1000);
			//console.log("clearing");
		},1);
	}
	
	this.draw = function(x,y){
		setTimeout(function(){
			var c = document.getElementById("can");
			var ctx = c.getContext("2d");
			ctx.fillStyle="#FFFFFF";
			ctx.fillRect(x,y,1,1);
			//console.log(y+"drawing"+x);
		},1);
		//console.log("finished drawing");
	}
}
var a = new game();
a.clear();
var b = new population(a);
b.reset();
b.create_population();
for (var k = 0;k < 20;k++){
	a.clear();
	console.log("thinking");
	for (var i = 0;i < 500;i++){
		for (var j = 0;j < b.Population.length;j++){
			b.think(b.Population[j]);
			a.draw(b.Population[j].x,b.Population[j].y);
		}
	}
	b.evolve();
}