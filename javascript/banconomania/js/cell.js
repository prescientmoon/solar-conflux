class Cell {
    constructor(texture,x,y) {
        this.texture = texture;
        this.s = "Air";
        this.m = 0;
        this.cords = new Victor(x,y);
    }
    findNeighbors(accepted=[],exceptions=[]){
        const cords = this.cords;
        let res = [];
        for (var i = cords.x - 1; i <= cords.x + 1; i++) {
            for (var j = cords.y - 1; j <= cords.y + 1; j++) {
                if (i != -1 && j != -1 && i < game.grid.size.x && j < game.grid.size.y){
                    let obj = game.grid.map[i][j];
                    if (accepted.indexOf(obj.s) != -1 &&
                    exceptions.indexOf(obj.s) == -1)
                        res.push(obj);
                }
            }
        }
        return res;
    }
    get mps(){
        if (this.s == "Tower"){
            if (this.findNeighbors(["Tower"]).length > 1){
                return this.m;
            }
            return 0;
        }
        else if (this.s == "Factory" || this.s == "bFactory"){
            if (this.findNeighbors(["Air"]).length == 8){
                return this.m;
            }
            return 0;
        }
        else if (this.s == "School"){
            let neis = this.findNeighbors(["House","Tower","Factory","bFactory"],"School");
            let res = 0;
            for (let i of neis)
                res += i.mps;
            return res;
        }
        return this.m;
    }
    set mps(value){
        this.m = value;
    }
    get state(){
        return this.s;
    }
    set state(value){
        const building = getBuildingByName(value);
        this.s = value;
        this.texture = loadImage(building.url);
        this.m = building.mps;
    }
}
