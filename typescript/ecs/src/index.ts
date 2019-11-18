import { System } from './types/System'
import { CompactComponentManager } from './classes/CompactComponentManager'
import { Ecs } from './classes/Ecs'

const ecs = new Ecs({
  name: new CompactComponentManager<string>()
})

class PunctuationSystem implements System<string> {
  public onUpdate(component: string, setComponent: (value: string) => void) {
    console.log(`Updating: ${component}`)

    setComponent(component + '!')
  }

  public onRender(component: string) {
    console.log(component)
  }
}

ecs.registerSystem(PunctuationSystem, 'name')

const foo = ecs.create({ name: 'foo' })
const bar = ecs.create({ name: 'bar' })

// Usually call these in a loop
ecs.update()
ecs.render()

ecs.destroy(foo, bar)
