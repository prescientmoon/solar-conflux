const defaultCellTexture = new Image(30,30);
defaultCellTexture.src = "textures/grass.png";
function EmptyMap(size){
    const object = new Array(size.x);
    for (var i = 0; i < size.x; i++){
        object[i]  = [];
        for (var j = 0; j < size.y; j++){
            object[i][j] = new Cell(defaultCellTexture,i,j);
        }
    }
    return object;
}
class Grid{
    constructor(cols,rows){
        this.size = new Victor(cols,rows);
        this.map = EmptyMap(this.size);
    }
    set Asize (value){
        this.size = value;
        cellSize = canvasSize.clone().divide(game.grid.size);
        const oldGrid = this.map;
        this.map = EmptyMap(this.size);
        for (var i = 0; i < oldGrid.length; i++) {
            for (var j = 0; j < oldGrid[i].length; j++) {
                this.map[i][j] = oldGrid[i][j];
            }
        }
    }
}
