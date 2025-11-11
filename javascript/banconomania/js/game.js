const buildingData = [
    {
        name:"Air",
        cost:0,
        url:"textures/grass.png",
        mps:0,
        description:`The air is useless for making money...<br/>
         But you can replace other things wwith it:)<br/>
         You better go and build something XD<br/>
         You know? You can't get rich without sacrificing some money in the first place :3`
    },
    {
        name:"House",
        cost:200,
        url:"textures/house.png",
        mps:10,
        description:`A house for some normal people...<br/>
         Those won't get you a lot of money, but hey, everyone starts somewhere!`
    },
    {
        name:"Tower",
        cost:400,
        url:"textures/tower.png",
        mps:50,
        description:`Getting into more serious stuff... The towers can generate more money...<br/>
        BUT WITH A TWIST: you must place at least 2 of them in adiacent spots for them to generate any money!`
    },
    {
        name:"Factory",
        cost:2000,
        url:"textures/factory.png",
        mps:200,
        description:`A real money source uses resources. The factory uses water to get you money.<br/>
        You guessed it :3 There is a twist! Each factory needs to have freee spots all around it!`
    },
    {
        name:"bFactory",
        cost:10000,
        url:"textures/big_factory.png",
        mps:2000,
        description:`A bigger version of the factory, usies lava as fuel.The same requirements`
    },
    {
        name:"School",
        cost:100000,
        url:"textures/school.png",
        mps:"?",
        description:`Nobody likes to go to school. It doesn't get you any money...<br/>
        But it doubles the profit of all near buildings XD`
    }
];

function getBuildingByName(name){
    for (let i of buildingData){
        if (i.name == name)
            return i;
    }
    return undefined;
}

class Game{
    constructor(grid) {
        this.grid = grid;
        console.log(this.grid);
        this.money = 1000;
        this.selected = 0;
        this.goal = 1000;
        this.goals = [1000,5000,10000];
        this.goalSizes  = [new Victor(7,7),new Victor(10,10),new Victor(20,20)];
    }
    update(delta){
        if (this.money > this.goal){
            this.grid.Asize = this.goalSizes[this.goals.indexOf(this.goal)];
            this.goal = this.goals[this.goals.indexOf(this.goal) + 1];
        }
        let tmps = 0;
        for (let i of this.grid.map){
            for (let j of i){
                tmps += j.mps;
            }
        }
        this.money += delta * tmps / 1000;
        this.updateMoney(tmps);
    }
    handleClick(eventPosition){
        console.clear();
        console.table(eventPosition.toArray());
        const sh = buildingData[this.selected];
        if (this.money >= sh.cost){
            this.money -= sh.cost;
            this.grid.map[eventPosition.y][eventPosition.x].state = sh.name;
        }
    }
    updateMoney(tmps){
        $("#money").html(this.moneyString(this.money));
        //total money per second
        $("#mps").html(this.moneyString(tmps));
    }
    moneyString(value){
        var floor = Math.floor(value).toString();
        var res;
        if (floor > 1000000){
            res = `${Math.floor(floor/1000000)}M`;
        }
        else if (floor > 1000){
            res = `${Math.floor(floor/1000)}K`;
        }
        else {
            res = `${floor}$`;
        }
        return res;
    }
}
