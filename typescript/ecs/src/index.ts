import { CompactComponentManager } from './classes/CompactComponentManager'
import { Ecs } from './classes/Ecs'
import { MappedComponentManager } from './classes/MappedComponentManager'

const ecs = new Ecs({
  age: new CompactComponentManager<number>(),
  name: new CompactComponentManager<string>(),
  nickname: new MappedComponentManager<string>()
})

const foo = ecs.create({
  name: 'Foo',
  age: 7,
  nickname: 'Tom'
})

const unnamed = ecs.create({
  age: 9
})

const bar = ecs.create({
  name: 'Bar',
  nickname: 'Jerry'
})

const result = ecs.pickComponentByEid(unnamed, ['age'])

console.log(result)

for (const age of ecs.components.age) {
  console.log(age)
}

for (const nickname of ecs.components.nickname) {
  console.log(nickname)
}
