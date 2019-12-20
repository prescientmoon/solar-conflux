const canvas = document.getElementById('canvas');
var performancLimit = 2500;
const ctx = canvas.getContext("2d");
const MainPlayer = new Player(ctx,400,200);
const MainCanvas = new Turtle(ctx);
const MainSpawner = new Spawner();
var score = 5;
var paused = false;
var h = 0,w = 0;
var particles = [],walls = [],enemies = [],bullets = [],arcs = [];
new Wall(ctx,20,50,40,50);
new Wall(ctx,70,50,90,50);
new Wall(ctx,0,0,100,0);
new Wall(ctx,0,0,0,100);
new Wall(ctx,100,0,100,100);
new Wall(ctx,0,100,100,100);
const background = "#000000";
const size = [1600,900];
const bossManager = new boss();
//bossManager.w37();
//createEnemy(plasma);
function die(){
	$("#GO-text").html(`You died...
		<br/> Score:${score}\n
		<br/> Wave:${MainSpawner.wave}`);
	$("#GO").fadeIn(1000);
	$("#GO-text").fadeIn(1500);
}
