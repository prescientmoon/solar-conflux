const Mongoose = require("mongoose");

class Mongy{
    constructor(){
        this.connected = false;

        this.Schemas = {};
        this.Models = {};
    }
    createModel(name,collection,structure){
        this.Schemas[name] = new Mongoose.Schema(structure);
        this.Models[name] = new Mongoose.model(name,this.Schemas[name],collection);
    }
    instantiate(name,data){
        return new this.Models[name](data);
    }
    saveModel(model){
        return new Promise((resolve, reject) => {
            model.save((err,res) => {
                if (err) reject(err);

                resolve(res);
            });
        });
    }
    async connect(url){
        Mongoose.connect(url,{
            useNewUrlParser: true
        });
        this.db = Mongoose.connection;

        await this.open();

        this.connected = true;
    }
    async open(){
        const {db} = this;
        return new Promise((resolve, reject) => {
            db.once('open', function() {
                resolve();
            });
        });
    }
    find(modelName,query){
        const model = this.Models[modelName];

        return new Promise((resolve,reject) => {
            model.find(query,(error,result) => {
                if (error) reject(error);
                resolve(result);
            });
        });
    }
    remove(modelName,query){
        const model = this.Models[modelName];
        return new Promise((resolve,reject) => {
            model.find(query).remove(() => {
                resolve(true);
            });
        });
    }
}

module.exports = Mongy;
