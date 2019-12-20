var started = false;
var tutorial = true;
var abilityTuroial = true;
$("#start,#end,#credits").mousedown((e) => {
	e.preventDefault();
});
$("#start").click((e) => {
	$("#main-menu,#credit-screen").fadeOut(100,(e) => {
		$("#canvas").fadeIn(100);
		resize();
		//MainLoop.start();
		//MainLoop.stop();
		if (started)
			return 0;
		started = true;
		const func = () => {
			dialog(`AFG: Oh... I think i found something!       <br/>
				    There is a known black whole nearby!!!       <br/>
				    We can use it to warp us close to terran territory!       <br/>
				    It's not really that close actually... it's 37 Sectors away.       <br/>
				    The radar detects hordes of unknown enemies rappidly approaching us...`,30,true,() => {
				    	dialog(`The ship is still damaged but it will recover in time, rebooting the advanced weaponry systems on board.       <br/>
				    		But you have to fight them like this until i get everything set up properly.       <br/>
				    		Good luck!`,50,true,() => {
				    			$("#dialog").fadeOut(300);
				    			MainLoop.start();
				    		});
				    });
		};
		dialog(`WARNING!!!             <br/> 
			System Failure!!!             <br/>
		    WARNING!!!             <br/>
			Rebooting...`,50,true,() => {
			dialog(`
				Reboot succesfull!             <br/> 
				Powering up AFG...`,100,true,() => {
				dialog(`AFG: Hi.             <br/> 
					This is your automatic flight guidance system, AFG for short.             <br/> 
					It appears you have warped out of charts.             <br/> 
				    You must find your way back home...`,50,true,() => {
				    	dialog(`Before i try to find a solution,    <br/>
				    		let's get you started with the controls:`,75,true,() => {
				    			dialog(`A&D - rotate     <br/>
										SPACE&W - accelerate     <br/>
			    						S - shoot     <br/>
			    						`,50,true,func);
				    		});
				    });
			});
		});
		$("iframe").attr("allow","");
		$("iframe").attr("src","");
		$("#left,#right").attr("class","control");
		$("#left").tapstart((e) => {
			MainPlayer.rotationFriction = 0.98;
			MainPlayer.rotationAccel = -MainPlayer.RotationThrust;
		});
		$("#right").tapstart((e) => {
			MainPlayer.rotationFriction = 0.98;
			MainPlayer.rotationAccel = MainPlayer.RotationThrust;
		});
		$("#left").tapend((e) => {
			MainPlayer.rotationFriction = 0.88;
			MainPlayer.rotationAccel = 0;
		});
		$("#right").tapend((e) => {
			MainPlayer.rotationFriction = 0.88;
			MainPlayer.rotationAccel = 0;
		});
		const song = document.getElementById('main-song');
		song.play();
	});
});
$("#credits").click((e) => {
	$("#credit-screen").fadeIn(500,() => {
		$("#credit-screen").css("display","inline");
	});
});
$("#credit-close").click((e) => {
	$("#credit-screen").fadeOut(500);
});
$("body").keydown(function(e){
	if (!paused){
		if (e.keyCode == 32){
			MainPlayer.isThrusting = true;
		}
		if (e.keyCode == 65){
			MainPlayer.rotationFriction = 0.98;
			MainPlayer.rotationSpeed -= MainPlayer.RotationThrust;
		}
		if (e.keyCode == 68){
			MainPlayer.rotationFriction = 0.98;
			MainPlayer.rotationSpeed += MainPlayer.RotationThrust;
		}
		if (e.keyCode == 83){
			MainPlayer.shoot();
		}
		if (e.keyCode == 81){
			MainPlayer.bomb();
		}
		if (e.keyCode == 69){
			MainPlayer.heal();
		}
	}
	if (e.keyCode == 49){
		openFullscreen();
	}
	//console.log(e.keyCode);
	e.preventDefault()
});
$("body").keyup(function(e){
	if (!paused){
		if (e.keyCode == 32){
			MainPlayer.isThrusting = false;
		}
		else if (e.keyCode == 65){
			MainPlayer.rotationFriction = 0.88;
		}
		else if (e.keyCode == 68){
			MainPlayer.rotationFriction = 0.88;
		}
	}
	if (e.keyCode == 80){
		if (!paused){
			paused = true;
			MainLoop.stop();
		}
		else{
			paused = false;
			MainLoop.start();
		}
	}
	e.preventDefault()
});
$("body").mousedown(function(e){
	if (!paused){
		if (e.button == 2){
			//MainPlayer.isThrusting = true;
		}
		else if (e.button == 0){
			//MainPlayer.shoot();
		}
	}
});
$("body").mouseup(function(e){
	if (e.button == 2){
		MainPlayer.isThrusting = false;
	}
});
$(window).resize(resize);

function resize(){
	if ($("body").width() > $("body").height()){
		$("#canvas").attr("width",($("body").height() * 16 / 9).toString());
		$("#canvas").attr("height",$("body").height().toString());
	}
	h = $("#canvas").height();
	w = $("#canvas").width();
}

$("#dialog").click((e) => {
	if (dialoged.absolute){
		if (!dialoged.relative){
			if (MainSpawner.wave == 37){
				W37start();
				enemies = [];
			}
			dialoged.absolute = false;
			return 0;
		}
		else
			dialoged.relative = false;
	}
	console.table(dialoged);
});

var elem = document.documentElement;

/* View in fullscreen */
function openFullscreen() {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}
 