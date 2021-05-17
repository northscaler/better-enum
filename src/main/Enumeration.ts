import isValidJavaScriptIdentifier from './is-valid-javascript-identifier'
import {
  DuplicateEnumerationDeclarationError,
  InvalidEnumerationNameError,
  InvalidEnumerationOrdinalError,
  UnidentifiableEnumerationValueError,
  UnknownEnumerationClassError,
} from './errors'

type EnumerationsByName = { [name: string]: any }
type EnumerationsByOrdinal = { [ordinal: number]: any }

// key type is really a class function:  that is, `Foo` reference of `class Foo {}`
const values = new Map<
  any,
  { name: EnumerationsByName; ordinal: EnumerationsByOrdinal }
>()

/**
 * Base enumeration class for the pattern where enumerated values are better represented by instance of classes rather than scalar values.
 *
 * Every subclass `T` has the following requirements.
 *
 * * `T` must not be extended.
 *
 * * `T` must define a `private` constructor whose first two arguments are `name: string, ordinal: number`,
 *   * where `name` is a valid JavaScript identifier that is the same as the instance's static identifier on `T`, and
 *   * where `ordinal` is an integral value that is unique amongst all instances.
 *
 * * `T` must define a `static` method `of(it: T | string | number): T` that returns an instance of `T` whose `name` or `ordinal` is equal to the given value, or that is identical to the instance of `T` given, and must throw {@link UnidentifiableEnumerationValueError} otherwise.
 *
 * > NOTE: {@link Enumeration} provides a convenient function that subclasses can delegate to in order to implement the required `of` method above.
 * > Use `import { _of } from '@northscaler/better-enum' and delegate your `of` method to it.
 *
 * * `T` must define a `static` method `values(): T[]` that returns all instances of `T`.
 *
 * > NOTE: {@link Enumeration} provides a convenient function that subclasses can delegate to in order to implement the required `values` method above.
 * > Use `import { _values } from '@northscaler/better-enum' and delegate your `values` method to it.
 */
export class Enumeration<T extends Enumeration<T>> {
  /**
   * Constructs an enumerated value.
   * @param _name The symbolic name of this instance.
   * It must be a valid JavaScript symbol and must be unique amongst all instances of this class's type.
   * @param _ordinal The numeric ordinal of this instance.
   * It must be an integer value and must be unique amongst instances of this class's type.
   * @param _class The class function of the subclass calling this constructor.
   * For example, given `class Bool extends Enumeration<Bool> { ... }`, the reference to the class's function is `Bool`.
   * @throws InvalidEnumerationNameError If the `_name` parameter is not a valid JavaScript identifier.
   * @throws InvalidEnumerationOrdinalError If the `_ordinal` parameter is not an integer.
   * @throws DuplicateEnumerationDeclarationError If there is already a value with the given `_name` or `_ordinal` amongst all instances of this class's type.
   * @protected
   */
  protected constructor(
    protected _name: string,
    protected _ordinal: number,
    // eslint-disable-next-line @typescript-eslint/ban-types
    protected _class: Function
  ) {
    if (
      Object.getPrototypeOf(Object.getPrototypeOf(this)).constructor.name !==
      Enumeration.name
    ) {
      throw new Error('farts')
    }

    if (!isValidJavaScriptIdentifier(_name)) {
      throw new InvalidEnumerationNameError({ context: { name: _name } })
    }

    const n = parseInt(String(_ordinal))
    if (isNaN(n) || n !== _ordinal) {
      throw new InvalidEnumerationOrdinalError({
        context: { ordinal: _ordinal },
      })
    }

    let clazz = values.get(this._class)
    if (!clazz) {
      values.set(this._class, (clazz = { name: {}, ordinal: {} }))
    }
    if (clazz!.name[_name] || clazz!.ordinal[_ordinal]) {
      throw new DuplicateEnumerationDeclarationError({
        context: { name: _name, ordinal: _ordinal },
      })
    }

    clazz!.name[_name] = clazz!.ordinal[_ordinal] = (this as unknown) as T
  }

  /**
   * The symbolic name of this enumerated value.
   */
  name() {
    return this._name
  }

  /**
   * The integer ordinal of this enumerated value.
   */
  ordinal() {
    return this._ordinal
  }

  /**
   * Returns whether the given object has the same name, ordinal & constructor name as this object.
   */
  equals(that: T) {
    return (
      this.constructor.name === that.constructor.name &&
      this.name() === that.name() &&
      this.ordinal() === that.ordinal()
    )
  }

  /**
   * Returns the symbolic name of this enumerated value.
   * @param fullyQualified Whether to return the return value of {@link toFullyQualifiedString} or just the symbolic name; defaults to `false`.
   */
  toString(fullyQualified = false) {
    return fullyQualified ? this.toFullyQualifiedString() : this._name
  }

  /**
   * Returns a colon-separated string of this class's name, its symbolic name, and its ordinal (like `Bool:TRUE:1`).
   */
  toFullyQualifiedString() {
    return `${this.constructor.name}:${this._name}:${this._ordinal}`
  }
}

/**
 * A function that attempts to locate an enumerated value given its name or ordinal.
 * If given an argument that is `typeof object`, it must be an enumerated value, which is simply returned.
 * Any value that is not among the symbolic names, ordinals, or instances of the enumeration class will cause this method to throw an `UnidentifiableEnumerationValueError`.
 *
 * This function is only intended to be used by subclasses of {@link Enumeration}.
 *
 * @param it The value being used to identify the enumerated value.
 * @param clazz A reference to the enumeration class (if `class Foo ... {}`, then the reference `Foo` should be given).
 */
export function _of<T>(
  it: T | string | number,
  // eslint-disable-next-line @typescript-eslint/ban-types
  clazz: Function
): T {
  let e: T
  const type = typeof it

  const f = values.get(clazz)
  if (!f) {
    throw new UnknownEnumerationClassError({ context: { clazz } })
  }

  if (type === 'string') {
    e = f!.name[it as string]
  } else if (type === 'number') {
    e = f!.ordinal[it as number]
  } else if (it instanceof clazz) {
    e = it as T
  }

  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  if (e) {
    return e
  }

  throw new UnidentifiableEnumerationValueError({
    message: 'unidentifiable enumeration value',
    context: { value: it },
  })
}

// eslint-disable-next-line @typescript-eslint/ban-types
export function _values(clazz: Function) {
  const f = values.get(clazz)

  if (!f) {
    throw new UnknownEnumerationClassError({ context: { clazz } })
  }

  return Object.values(f.name)
}
