import { UmbrellaComponentManager } from './UmbrellaComponentManager'
import { CompactComponentManager } from './CompactComponentManager'
import { MappedComponentManager } from './MappedComponentManager'
import {
  ComponentManager,
  ComponentManagerClass
} from '../types/ComponentManager'
import { expect } from 'chai'
import { VectorComponentManager } from './VectorComponentManager'
import { Ecs } from './Ecs'
import { typedManager } from '../helpers/typedManager'

for (const Manager of [
  UmbrellaComponentManager,
  CompactComponentManager,
  MappedComponentManager,
  VectorComponentManager
]) {
  const name = Manager.toString().split(' ')[1]

  describe(`The ${name} instance`, () => {
    let manager: ComponentManager<number>

    beforeEach(() => {
      const ecs = new Ecs({
        foo: typedManager<number>(Manager)
      })
      manager = ecs.components.foo

      if (Manager === VectorComponentManager) {
        VectorComponentManager.clear()
      }

      if (manager.init) {
        manager.init(ecs)
      }
    })

    describe('The getComponentByEid method', () => {
      it('should throw an error if the component doesnt exist', () => {
        // assert
        expect(() => manager.getComponentByEid(0)).to.throw()
      })

      it('should return the correct value if the component exists', () => {
        // arrange
        const random = Math.random()
        manager.register(0, random)

        // act
        const result = manager.getComponentByEid(0)

        // assert
        expect(result).to.equal(random)
      })
    })

    it('should allow registering a component', () => {
      // arrange
      const random = Math.random()

      // act
      manager.register(7, random)

      // assert
      expect(manager.getComponentByEid(7)).to.equal(random)
    })
  })
}
