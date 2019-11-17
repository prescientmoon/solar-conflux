export class EidGenerator {
  private generations: number[] = []
  private unused: number[] = []
  private indexMask: number
  private generationMask: number

  public constructor(private indexBits = 22, bits = 32) {
    this.indexMask = (1 << indexBits) - 1
    this.generationMask = (1 << (bits - indexBits)) - 1
  }

  private makeEntity(index: number, generation: number = 0) {
    return (generation << this.indexBits) | index
  }

  public getGenerationById(eid: number) {
    return (eid >> this.indexBits) & this.generationMask
  }

  public getIndexById(eid: number) {
    return eid & this.indexMask
  }

  public create() {
    if (this.unused.length) {
      const index = this.unused.shift()!

      return this.makeEntity(index, this.generations[index])
    }

    return this.generations.push(0) - 1
  }

  public destroy(eid: number) {
    const index = eid & this.indexMask

    // make sure the entity was created
    if (index >= this.generations.length) {
      throw new Error(`Cannot destroy unallocated index ${index}`)
    }

    // Make sure the entity is alive
    if (!this.isAlive(eid)) {
      throw new Error('Cannot destroy an entity more than once')
    }

    const generation = this.generations[index]

    // Make sure we can reuse the entity
    if (((generation + 1) & this.generationMask) === generation + 1) {
      this.generations[index]++
      this.unused.push(index)
    }
  }

  public isAlive(eid: number) {
    const index = eid & this.indexMask
    const generation = this.getGenerationById(eid)

    return this.generations[index] === generation
  }
}
