import { _of, _values, Enumeration } from '../../main'

let ordinal = 1

// NOTE: directly from enum Planet at https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

export default class Planet extends Enumeration<Planet> {
  // universal gravitational constant (m3 kg-1 s-2)
  static readonly G = 6.673e-11

  static readonly MERCURY = new Planet('MERCURY', ordinal++, 3.303e23, 2.4397e6)
  static readonly VENUS = new Planet('VENUS', ordinal++, 4.869e24, 6.0518e6)
  static readonly EARTH = new Planet('EARTH', ordinal++, 5.976e24, 6.37814e6)
  static readonly MARS = new Planet('MARS', ordinal++, 6.421e23, 3.3972e6)
  static readonly JUPITER = new Planet('JUPITER', ordinal++, 1.9e27, 7.1492e7)
  static readonly SATURN = new Planet('SATURN', ordinal++, 5.688e26, 6.0268e7)
  static readonly URANUS = new Planet('URANUS', ordinal++, 8.686e25, 2.5559e7)
  static readonly NEPTUNE = new Planet('NEPTUNE', ordinal++, 1.024e26, 2.4746e7)

  static of(it: Planet | string | number) {
    return _of(it, Planet)
  }

  static values() {
    return _values<Planet>(Planet)
  }

  constructor(
    name: string,
    ordinal: number,
    private _mass: number,
    private _radius: number
  ) {
    super(name, ordinal, Planet)
  }

  get mass() {
    return this._mass
  }

  get radius() {
    return this._radius
  }

  get surfaceGravity() {
    return (Planet.G * this._mass) / (this._radius * this._radius)
  }

  surfaceWeight(otherMass: number) {
    return otherMass * this.surfaceGravity
  }
}
