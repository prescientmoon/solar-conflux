$("#canvas").click((e) => {
    console.log("Click dettected!!!");
    let x = (e.clientX - camera.x) / camera.sx;
    let y = (e.clientY - camera.y) / camera.sy;
    const eventPosition = new Victor(
        Math.floor(x/cellSize.x),
        Math.floor(y/cellSize.y));
    game.handleClick(eventPosition);
});
$("#building").change((e) => {
    const res = parseFloat($("#building option:selected").val());
    game.selected = res;
    $("#description").html(buildingData[game.selected].description);
    $("#cost").html(game.moneyString(buildingData[game.selected].cost));
});
$("body").keydown((e) => {
    let a = e.keyCode;
    if (a == 68)
        camera.x -= camera.movementSpeed;
    else if (a == 65)
        camera.x += camera.movementSpeed;
    if (a == 87)
        camera.y += camera.movementSpeed;
    else if (a == 83)
        camera.y -= camera.movementSpeed;
    if (a == 189){
        camera.sx /= camera.zoomSpeed;
        camera.sy /= camera.zoomSpeed;
    }
    else if (a == 187){
        camera.sx *= camera.zoomSpeed;
        camera.sy *= camera.zoomSpeed;
    }
    console.log(e.keyCode);
});
$("body").mousewheel((e) => {
    console.log(e);
    if (e.deltaY == 1){
        camera.sx *= camera.zoomSpeed;
        camera.sy *= camera.zoomSpeed;
    }
    if (e.deltaY == -1){
        camera.sx /= camera.zoomSpeed;
        camera.sy /= camera.zoomSpeed;
    }
});

$("#b1").click((e) => {
    const audio = document.getElementById('song');
    audio.play();
    $("#start").fadeOut(1000);
    $("#canvas").fadeIn(1000);
    $("#container").fadeIn(1000);
});
$("#b2").click((e) => {
    alert(`Move camera: WASD
Zoom: scroll/ - & +
Place selected building: click on tile`);
    //$("<div>Tutorial</div>").dialog({});
});
$("#b3").click((e) => {
    alert(`Programming: Matei Adriel
Art: Matei Adriel
Music: Calin Avram`);
});
$("#b4").click((e) => {
    window.close();
});
