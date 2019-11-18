import { EidGenerator } from './EidGenerator'
import { expect } from 'chai'

describe('The EidGenerator instance', () => {
  let eidGenerator: EidGenerator

  beforeEach(() => {
    eidGenerator = new EidGenerator()
  })

  describe('The create method', () => {
    it('Should return a different eid each time', () => {
      const ids = new Set<number>()

      for (let index = 0; index < 10000; index++) {
        const eid = eidGenerator.create()

        expect(ids.has(eid)).to.be.false

        ids.add(eid)
      }
    })
  })

  describe('The destroy method', () => {
    it('Should throw if the entity was never created', () => {
      expect(() => eidGenerator.destroy(0)).to.throw()
    })

    it("Should throw if the entity wasn't alive", () => {
      const eid = eidGenerator.create()
      eidGenerator.destroy(eid)

      expect(() => eidGenerator.destroy(eid)).to.throw()
    })

    it('Should mark the entity as dead if it was alive', () => {
      const eid = eidGenerator.create()

      eidGenerator.destroy(eid)

      expect(eidGenerator.isAlive(eid)).to.be.false
    })
  })

  describe('The isAlive method', () => {
    it('Should return false if the entity was never created', () => {
      expect(eidGenerator.isAlive(0)).to.be.false
    })

    it('Should return true if the entity was just created', () => {
      const eid = eidGenerator.create()

      expect(eidGenerator.isAlive(eid)).to.be.true
    })

    it('Should return false if the entity was just destroyed', () => {
      const eid = eidGenerator.create()

      eidGenerator.destroy(eid)

      expect(eidGenerator.isAlive(eid)).to.be.false
    })
  })
})
