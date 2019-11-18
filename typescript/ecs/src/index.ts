import { CompactComponentManager } from './classes/CompactComponentManager'
import { Ecs } from './classes/Ecs'
import { MappedComponentManager } from './classes/MappedComponentManager'
import { System } from './classes/System'

const ecs = new Ecs({
  age: new CompactComponentManager<number>(),
  name: new CompactComponentManager<string>(),
  nickname: new MappedComponentManager<string>()
})

class AgeSystem extends System<number> {
  public didCreate(component: number) {
    console.log(`Recived component: ${component}`)
  }
}

class NameSystem extends System<string> {
  public didCreate(component: string) {
    console.log(`Recived name: ${component}`)
  }
}

ecs.registerSystem(AgeSystem, 'age')
ecs.registerSystem(NameSystem, 'name')

ecs.create({ name: 'somethign' })
ecs.create({ age: 7 })
ecs.create({ age: 123, name: 'something' })
