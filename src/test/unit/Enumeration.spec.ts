import * as chai from 'chai'
import Bool from './Bool'
import { Enumeration, sortByName, sortByOrdinal } from '../../main'
import {
  DuplicateEnumerationDeclarationError,
  InvalidEnumerationNameError,
  InvalidEnumerationOrdinalError,
  UnidentifiableEnumerationValueError,
} from '../../main/errors'
import HelloWorld from './HelloWorld'
import Planet from './Planet'

const expect = chai.expect

describe('unit tests of Enumeration', function () {
  it('should work normally with hello world', () => {
    expect(HelloWorld.of('HELLO')).to.equal(HelloWorld.HELLO)
    expect(HelloWorld.of('WORLD')).to.equal(HelloWorld.WORLD)
    expect(HelloWorld.of(0)).to.equal(HelloWorld.HELLO)
    expect(HelloWorld.of(1)).to.equal(HelloWorld.WORLD)
    expect(HelloWorld.of(HelloWorld.WORLD)).to.equal(HelloWorld.WORLD)
    expect(HelloWorld.of(HelloWorld.HELLO)).to.equal(HelloWorld.HELLO)
    expect(`${HelloWorld.WORLD}`).to.equal('WORLD')
    expect(HelloWorld.WORLD.toFullyQualifiedString()).to.equal(
      'HelloWorld:WORLD:1'
    )
    expect(
      HelloWorld.WORLD.equals({
        constructor: { name: 'HelloWorld' },
        name: () => 'WORLD',
        ordinal: () => 1,
      } as HelloWorld)
    ).to.be.true
    expect(() => HelloWorld.of('')).to.throw(
      UnidentifiableEnumerationValueError
    )
    expect(() => HelloWorld.of(-42)).to.throw(
      UnidentifiableEnumerationValueError
    )
    expect(JSON.stringify(Bool.TRUE)).to.equal('"TRUE"')

    expect(JSON.stringify(Bool.values().sort(sortByName))).to.equal(
      '["FALSE","NEITHER","TRUE"]'
    )
    expect(JSON.stringify(Bool.values().sort(sortByOrdinal))).to.deep.equal(
      '["NEITHER","FALSE","TRUE"]'
    )
  })

  it('should work normally', () => {
    expect(Bool.of('FALSE')).to.equal(Bool.FALSE)
    expect(Bool.of('TRUE')).to.equal(Bool.TRUE)
    expect(Bool.of(0)).to.equal(Bool.FALSE)
    expect(Bool.of(1)).to.equal(Bool.TRUE)
    expect(Bool.of(Bool.TRUE)).to.equal(Bool.TRUE)
    expect(Bool.of(Bool.FALSE)).to.equal(Bool.FALSE)
    expect(`${Bool.TRUE}`).to.equal('TRUE')
    expect(Bool.TRUE.toFullyQualifiedString()).to.equal('Bool:TRUE:1')
    expect(
      Bool.TRUE.equals({
        constructor: { name: 'Bool' },
        name: () => 'TRUE',
        ordinal: () => 1,
      } as Bool)
    ).to.be.true
    expect(Bool.TRUE.definite).to.be.true
    expect(Bool.FALSE.definite).to.be.true
    expect(Bool.NEITHER.definite).to.be.false
    expect(() => Bool.of('')).to.throw(UnidentifiableEnumerationValueError)
    expect(() => Bool.of(2)).to.throw(UnidentifiableEnumerationValueError)

    expect(Bool.TRUE.toBoolean()).to.be.true
    expect(Bool.FALSE.toBoolean()).to.be.false
    expect(() => Bool.NEITHER.toBoolean()).to.throw()
  })

  it('should prevent incorrect enumerations', function () {
    expect(() => {
      class BoolWithDupOrdinal extends Enumeration<BoolWithDupOrdinal> {
        static readonly F = new BoolWithDupOrdinal('FALSE', 0)
        static readonly T = new BoolWithDupOrdinal('TRUE', 0) // dup ordinal

        private constructor(_name: string, _ordinal: number) {
          super(_name, _ordinal, BoolWithDupOrdinal)
        }
      }
    }).to.throw(DuplicateEnumerationDeclarationError)

    expect(() => {
      class BoolWithDupName extends Enumeration<BoolWithDupName> {
        static readonly F = new BoolWithDupName('FALSE', 0)
        static readonly T = new BoolWithDupName('FALSE', 1) // dup name

        private constructor(_name: string, _ordinal: number) {
          super(_name, _ordinal, BoolWithDupName)
        }
      }
    }).to.throw(DuplicateEnumerationDeclarationError)

    expect(() => {
      class BoolWithFloatOrdinal extends Enumeration<BoolWithFloatOrdinal> {
        static readonly F = new BoolWithFloatOrdinal('FALSE', 0)
        static readonly T = new BoolWithFloatOrdinal('TRUE', 0.1) // float ordinal

        private constructor(_name: string, _ordinal: number) {
          super(_name, _ordinal, BoolWithFloatOrdinal)
        }
      }
    }).to.throw(InvalidEnumerationOrdinalError)

    expect(() => {
      class BoolWithInvalidName extends Enumeration<BoolWithInvalidName> {
        static readonly F = new BoolWithInvalidName('FALSE', 0)
        static readonly T = new BoolWithInvalidName('1T', 1) // invalid name

        private constructor(_name: string, _ordinal: number) {
          super(_name, _ordinal, BoolWithInvalidName)
        }
      }
    }).to.throw(InvalidEnumerationNameError)
  })

  it('should work with Planet', function () {
    const earthWeight = 45 // kg
    const mass = earthWeight / Planet.EARTH.surfaceGravity
    for (const p of Planet.values().sort(sortByOrdinal)) {
      console.log(`Your weight on ${p} is ${p.surfaceWeight(mass)}`)
    }
  })
})
