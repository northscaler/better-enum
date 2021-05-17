import isValidJavaScriptIdentifier from './is-valid-javascript-identifier'
import {
  DuplicateEnumerationDeclarationError,
  IllegallyExtendedEnumerationError,
  InvalidEnumerationNameError,
  InvalidEnumerationOrdinalError,
  UnidentifiableEnumerationValueError,
  UnknownEnumerationClassError,
} from './errors'

type EnumerationsByName = { [name: string]: any }
type EnumerationsByOrdinal = { [ordinal: number]: any }

// key type is a class function; that is, given `class Foo {}`, then symbol `Foo`
const enums = new Map<
  // eslint-disable-next-line @typescript-eslint/ban-types
  Function,
  { byName: EnumerationsByName; byOrdinal: EnumerationsByOrdinal }
>()

/**
 * Base enumeration class for the pattern where enumerated values are represented by instances of classes rather than by scalar string or numeric values alone.
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
 * > Use `import { _of } from '@northscaler/better-enum' and delegate the subclass's `of()` method to it.
 *
 * * `T` must define a `static` method `values(): T[]` that returns all instances of `T`.
 *
 * > NOTE: {@link Enumeration} provides a convenient function that subclasses can delegate to in order to implement the required `values` method above.
 * > Use `import { _values } from '@northscaler/better-enum' and delegate the subclass's `values()` method to it.
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
      throw new IllegallyExtendedEnumerationError({ context: { this_: this } })
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

    let class_ = enums.get(this._class)
    if (!class_) {
      enums.set(this._class, (class_ = { byName: {}, byOrdinal: {} }))
    }
    if (class_!.byName[_name] || class_!.byOrdinal[_ordinal]) {
      throw new DuplicateEnumerationDeclarationError({
        context: { name: _name, ordinal: _ordinal },
      })
    }

    class_!.byName[_name] = class_!.byOrdinal[_ordinal] = (this as unknown) as T
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

  /**
   * Default `toJSON()` protocol method.
   * Returns the symbolic name of this enumerated value.
   * @see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify#tojson_behavior
   *
   * To implement a JSON [`reviver`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse#using_the_reviver_parameter), use the static `of` method defined on your class.
   */
  toJSON() {
    return this.name()
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
export function _of<T extends Enumeration<T>>(
  it: T | string | number,
  // eslint-disable-next-line @typescript-eslint/ban-types
  clazz: Function
): T {
  let e: T
  const type = typeof it

  const f = enums.get(clazz)
  if (!f) {
    throw new UnknownEnumerationClassError({ context: { clazz } })
  }

  if (type === 'string') {
    e = f!.byName[it as string]
  } else if (type === 'number') {
    e = f!.byOrdinal[it as number]
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
export function _values<T extends Enumeration<T>>(clazz: Function): T[] {
  const f = enums.get(clazz)

  if (!f) {
    throw new UnknownEnumerationClassError({ context: { clazz } })
  }

  return Object.values(f.byName)
}

/**
 * Returns a convenient function to sort enumerations by their names.
 */
export const sortByName = <T extends Enumeration<T>>(
  a: Enumeration<T>,
  b: Enumeration<T>
) => a.name().localeCompare(b.name())

/**
 * Returns a convenient function to sort enumerations by their ordinals.
 */
export const sortByOrdinal = <T extends Enumeration<T>>(
  a: Enumeration<T>,
  b: Enumeration<T>
) => a.ordinal() - b.ordinal()
