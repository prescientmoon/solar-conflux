const camera = {
    sx:1,
    sy:1,
    x:100,
    y:100,
    movementSpeed:10,
    zoomSpeed:2
}
const water = new Image(30,30);
water.src = "textures/water.png";
const lava = new Image(30,30);
lava.src = "textures/lava.png";
function update(delta){
    game.update(delta);
}
function draw(){
    let s = $("#canvas").width();
    $("#canvas").attr("width","0");
    $("#canvas").attr("width",s.toString());
    ctx.save();
    ctx.translate(camera.x,camera.y);
    ctx.scale(camera.sx,camera.sy);
    for (var i = 0; i < game.grid.map.length; i++) {
        for (var j = 0; j < game.grid.map[i].length; j++) {
            ctx.drawImage(game.grid.map[i][j].texture,
                j * cellSize.x,
                i * cellSize.x,
                cellSize.x,
                cellSize.x);
        }
    }
    for (var i = 0; i < game.grid.map.length; i++) {
        for (var j = 0; j < game.grid.map[i].length; j++) {
            let obj = game.grid.map[i][j];
            if (obj.s == "Tower"){
                ctx.beginPath();
                ctx.moveTo((j + 0.5) * cellSize.x,
                    (i + 0.5) * cellSize.y);
                for (let nei of obj.findNeighbors("Tower")){
                    ctx.lineTo((nei.cords.y + 0.5) * cellSize.x,
                        (nei.cords.x + 0.5) * cellSize.y);
                    ctx.lineTo((j + 0.5) * cellSize.x,
                        (i + 0.5) * cellSize.y);
                }
                ctx.stroke();
            }
            else if (obj.s == "Factory" && obj.findNeighbors("Air").length == 8){
                for (let nei of obj.findNeighbors("Air")){
                    ctx.drawImage(water,
                        nei.cords.y * cellSize.x,
                        nei.cords.x * cellSize.x,
                        cellSize.x,
                        cellSize.x);
                }
                ctx.stroke();
            }
            else if (obj.s == "bFactory" && obj.findNeighbors("Air").length == 8){
                for (let nei of obj.findNeighbors("Air")){
                    ctx.drawImage(lava,
                        nei.cords.y * cellSize.x,
                        nei.cords.x * cellSize.x,
                        cellSize.x,
                        cellSize.x);
                }
                ctx.stroke();
            }
        }
    }
    ctx.restore();
}
MainLoop.setUpdate(update).setDraw(draw);
