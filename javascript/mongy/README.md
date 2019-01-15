# Mongy
A high level interface for MongoDB built on top of Mongoose


#Get started:

To get started, first install the package:
```
npm install mongy
```
Then you need to include it in your script:
```
const Mongy = require("mongy");
```

The next step is to create an instance of the Mongy class:
```
const mydb = new Mongy();
```

To connect to your datebase you need to call the (async) 'connect' method:
```
mydb.connect(process.env.DBURL);
```

To create your first model call createModel(name,collection,schema):
```
mydb.createModel("User","users",{
  name:String,
  email:String
});
```

To make an instance of the model call 'instantiate(name,data)':
```
async function addUser(name,email){
  const myNewUser = mydb.instantiate("User",{name,email});
}
```

And then to save your model call the (async) function 'saveModel':
```
async function addUser(name,email){
  ...
  await mydb.saveModel(myNewUser);
}
```

Then you can get the data of your user via the '.find(name,data)' method:
```
async function getUsers(name){
  return await mydb.find("User",{name});
}
```
