import { EcsBuilder } from './EcsBuilder'
import { MappedComponentManager } from './MappedComponentManager'
import { typedManager } from '../helpers/typedManager'
import { expect } from 'chai'
import { UmbrellaComponentManager } from './UmbrellaComponentManager'
import { GroupComponentManager } from './GroupComponentManager'
import { ExtractComponentType } from '../types/ExtractComponentType'
import { ExtractEcsSpec } from '../types/ExtractEcsSpec'

describe('The ecs builder instance', () => {
  it('should produce an ecs with the correct components', () => {
    // arrange
    const builder = new EcsBuilder()

    // act
    const ecs = builder
      .addManager('position', typedManager<number>(MappedComponentManager))
      .addManager('color', typedManager<string>(UmbrellaComponentManager))
      .build()

    // assert
    expect(ecs.components.position).to.be.instanceOf(MappedComponentManager)
    expect(ecs.components.color).to.be.instanceOf(UmbrellaComponentManager)
  })

  it('should produce an ecs with the correct groups', () => {
    // arrange
    const builder = new EcsBuilder()
      .addManager('position', typedManager<number>(MappedComponentManager))
      .addManager('speed', typedManager<number>(MappedComponentManager))
      .addManager('color', typedManager<string>(MappedComponentManager))

    // act
    const ecs = builder
      .group('physics', ['position', 'speed'])
      .group('rendering', ['color', 'position'])
      .build()

    // assert
    expect(ecs.components.physics).to.be.instanceOf(GroupComponentManager)
    expect(ecs.components.rendering).to.be.instanceOf(GroupComponentManager)

    expect(
      (ecs.components.physics as GroupComponentManager<
        ExtractEcsSpec<typeof ecs>,
        'position' | 'speed'
      >).components
    ).to.deep.equal(['position', 'speed'])

    expect(
      (ecs.components.rendering as GroupComponentManager<
        ExtractEcsSpec<typeof ecs>,
        'position' | 'color'
      >).components
    ).to.deep.equal(['color', 'position'])
  })
})
