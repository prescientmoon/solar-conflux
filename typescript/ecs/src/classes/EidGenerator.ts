export class EidGenerator {
  private generations: number[] = []
  private unused: number[] = []
  private indexMask: number
  private generationMask: number

  /**
   * Low level class to generate entity ids.
   *
   * @param indexBits The number of bits to allocate for the indices.
   * @param bits The maximum number of bits to be used for the ids.
   */
  public constructor(private indexBits = 22, bits = 32) {
    this.indexMask = (1 << indexBits) - 1
    this.generationMask = (1 << (bits - indexBits)) - 1
  }

  /**
   * Low level method to glue an id and an index together.
   *
   * @param index The index of the entity.
   * @param generation The generation of the entity.
   */
  private makeEntity(index: number, generation: number = 0) {
    return (generation << this.indexBits) | index
  }

  /**
   * Returns the generation of an entity given it's id.
   *
   * @param eid The id of the entity.
   */
  public getGenerationById(eid: number) {
    return (eid >> this.indexBits) & this.generationMask
  }

  /**
   * Returns the index of an entity given it's id.
   *
   * @param eid The id of the entity.
   */
  public getIndexById(eid: number) {
    return eid & this.indexMask
  }

  /**
   * Generate an unique id.
   */
  public create() {
    if (this.unused.length) {
      const index = this.unused.shift()!

      return this.makeEntity(index, this.generations[index])
    }

    return this.generations.push(0) - 1
  }

  /**
   * Marks a certain index as unused
   * (The full id can't be used again, because the generation will change).
   *
   * @param eid The id of the entity to destroy.
   *
   * @throws If the entity was never created.
   * @throws If the entity isn't alive.
   */
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

  /**
   * Checks if an entity is alive.
   *
   * @param eid The id of the entity.
   */
  public isAlive(eid: number) {
    const index = eid & this.indexMask
    const generation = this.getGenerationById(eid)

    return this.generations[index] === generation
  }
}
