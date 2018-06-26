function population(game){
	this.max = 50;
	this.top = 4;
	this.game = game;
	
	if (this.max < this.top){
		this.top = this.max;
	}
	
	this.Population = [];
	this.scale = 200;
	
	this.reset = function(){
		this.iteration = 1;
		this.mutateRate = 1; 
		
		this.best_population = 0; 
		this.best_fitness = 0;
		this.best_score = 0;
	}
	
	this.create_population = function(){
		this.Population = [];
		
		for (var i=0; i<this.max; i++){
			var newUnit = new synaptic.Architect.Perceptron(5, 32, 4);
			
			newUnit.index = i;
			newUnit.fitness = 0;
			newUnit.score = 0;
			newUnit.isWinner = false;
			newUnit.x = 250;
			newUnit.y = 100;
			//newUnit.round = 0;
			
			this.Population.push(newUnit);
		}
	}
	
	this.think = function(unit){
		var inputs = [500,500,unit.x,unit.y,dist(unit.x,unit.y,500,500)];
		var outputs = this.Population[unit.index].activate(inputs);
		
		var max = outputs.indexOf((Math.max.apply( Math,outputs)));
		//console.log(Math.max.apply( Math,outputs));
			if (max == 0 && unit.y >= 0){
				unit.y -= 1;
			}
			else if (max == 2 && unit.y <= 500){
				unit.y += 1;
			}
			else if (max == 1 && unit.x <= 500){
				unit.x += 1;
			}
			else if (max == 3 && unit.x >= 0){
				unit.x -= 1;
			}
			//unit.round++;
		//this.game.draw(unit.x,unit.y);
		unit.fitness = dist(unit.x,unit.y,0,0);
		//console.log(max+"evolving"+outputs);
	}
	
	this.evolve = function(){
		var Winners = this.select();
		
		if (this.mutateRate == 1 && Winners[0].fitness < 0){
			this.create_population();
		}
		else{
			this.mutateRate = 0.2;
		}
		
		
		for (var i=this.top; i<this.max; i++){
			var parentA, parentB, offspring;
				
			if (i == this.top){
				parentA = Winners[0].toJSON();
				parentB = Winners[1].toJSON();
				offspring = this.crossOver(parentA, parentB);

			} else if (i < this.max-2){
				parentA = this.getRandomUnit(Winners).toJSON();
				parentB = this.getRandomUnit(Winners).toJSON();
				offspring = this.crossOver(parentA, parentB);
				
			} else {
				offspring = this.getRandomUnit(Winners).toJSON();
			}

			// mutate the offspring
			offspring = this.mutation(offspring);
			
			// create a new unit using the neural network from the offspring
			var newUnit = synaptic.Network.fromJSON(offspring);
			newUnit.index = this.Population[i].index;
			newUnit.fitness = 0;
			newUnit.score = 0;
			newUnit.isWinner = false;
			newUnit.x = 250;
			newUnit.y = 100;
			//newUnit.round = 0;
			
			// update population by changing the old unit with the new one
			this.Population[i] = newUnit;
		}
		
		this.Population.sort(function(unitA, unitB){
			return unitA.index - unitB.index;
		});
	}
	
	this.select = function(){
		var sortedPopulation = this.Population.sort(
			function(unitA, unitB){
				return unitB.fitness - unitA.fitness;
			}
		);
		
		for (var i=0; i<this.top; i++){
			this.Population[i].isWinner = true;
		}
		
		return sortedPopulation.slice(0, this.top);
	}
	
	this.crossOver = function(parentA, parentB){
		var cutPoint = this.random(0, parentA.neurons.length-1);
		
		for (var i = cutPoint; i < parentA.neurons.length; i++){
			var biasFromParentA = parentA.neurons[i]['bias'];
			parentA.neurons[i]['bias'] = parentB.neurons[i]['bias'];
			parentB.neurons[i]['bias'] = biasFromParentA;
		}
		
		cutPoint = this.random(0, parentA.connections.length-1);
		
		for (var i = cutPoint; i < parentA.connections.length; i++){
			var weightFromParentA = parentA.connections[i]['weight'];
			parentA.connections[i]['weight'] = parentB.connections[i]['weight'];
			parentB.connections[i]['weight'] = weightFromParentA;
		}
		return this.random(0, 1) == 1 ? parentA : parentB;
	}
	
	this.mutation = function(offspring){
		for (var i = 0; i < offspring.neurons.length; i++){
			offspring.neurons[i]['bias'] = this.mutate(offspring.neurons[i]['bias']);
		}
		
		for (var i = 0; i < offspring.connections.length; i++){
			offspring.connections[i]['weight'] = this.mutate(offspring.connections[i]['weight']);
		}
		
		return offspring;
	}
	
	this.mutate = function(gene){
		if (Math.random() < this.mutateRate) {
			var mutateFactor = 1 + ((Math.random() - 0.5) * 3 + (Math.random() - 0.5));
			gene *= mutateFactor;
		}
		
		return gene;
	}
	
	this.random = function(min, max){
		return Math.floor(Math.random()*(max-min+1) + min);
	}
	
	this.getRandomUnit = function(array){
		return array[this.random(0, array.length-1)];
	}
}


function dist(x1,y1,x2,y2){
	var x = x2 - x1;
	var y = y2 - y1;
	return Math.sqrt(Math.pow(x,2) + Math.pow(y,2));
}















