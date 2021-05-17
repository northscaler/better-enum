import { _of, _values, Enumeration } from '../../main'

let ordinal = -1

/**
 * A tristate boolean.
 */
export default class Bool extends Enumeration<Bool> {
  static readonly NEITHER = new Bool('NEITHER', ordinal++, false)
  static readonly FALSE = new Bool('FALSE', ordinal++, true)
  static readonly TRUE = new Bool('TRUE', ordinal++, true)

  static of(it: Bool | string | number): Bool {
    return _of(it, Bool)
  }

  static values(): Bool[] {
    return _values(Bool)
  }

  private constructor(
    _name: string,
    _ordinal: number,
    private _definite: boolean
  ) {
    super(_name, _ordinal, Bool)
  }

  /**
   * Returns `true` of this enum is {@link Bool.TRUE} or {@link Bool.FALSE}.
   * If this enum is {@link Bool.NEITHER}, returns `false`.
   */
  get definite() {
    return this._definite
  }

  /**
   * Converts this {@link Bool} to a `boolean` if it's `definite`, else throws an `Error`.
   */
  toBoolean() {
    if (this === Bool.TRUE) return true
    if (this === Bool.FALSE) return false
    throw new Error('neither true nor false')
  }
}
