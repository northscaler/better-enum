# `better-enum`

Better enumeration support for TypeScript than its `enum` keyword. This class is modeled
after [Java's enumeration pattern](https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html), where enums are
instances of classes. This library provides a base `Enumeration` class, along with some conventions and patterns
specific to the notion of enumerated values.

## TL;DR

Here's a very simple "Hello, World!" enumeration.

```typescript
import { _of, _values, Enumeration } from '@northscaler/better-enum' // 1️⃣ 

export default class HelloWorld extends Enumeration<HelloWorld> {    // 2️⃣
  static readonly HELLO = new HelloWorld('HELLO', 0)                 // 3️⃣
  static readonly WORLD = new HelloWorld('WORLD', 1)

  static of(it: HelloWorld | string | number): HelloWorld {          // 4️⃣
    return _of(it, HelloWorld)
  }

  static values(): HelloWorld[] {                                    // 5️⃣
    return _values<HellowWorld>(HelloWorld)
  }

  private constructor(name: string, ordinal: number) {               // 6️⃣
    super(name, ordinal, HelloWorld)
  }
}
```

1️⃣ Import Northscaler's enumeration support

2️⃣ Define your enum to extend `Enumeration`

3️⃣ Define your enum instances with their `name` & `ordinal` (additional, arbitrary properties are allowed, too)

4️⃣ Define a `static` `of()` method that converts from `string` names and `number` ordinals to the enum instance

5️⃣ Define a `static` `values()` method that returns all values of this enum type

6️⃣ Define a `private` constructor that takes the `name` & `ordinal` for an enum instance (again, additional properties
are allowed, too)

Now use it.

```typescript
// mocha test examples

expect(HelloWorld.of('HELLO')).to.equal(HelloWorld.HELLO)          // 1️⃣
expect(HelloWorld.of('WORLD')).to.equal(HelloWorld.WORLD)

expect(HelloWorld.of(0)).to.equal(HelloWorld.HELLO)                // 2️⃣
expect(HelloWorld.of(1)).to.equal(HelloWorld.WORLD)

expect(`${HelloWorld.WORLD}`).to.equal('WORLD')                    // 3️⃣

expect(HelloWorld.WORLD.toFullyQualifiedString()).to.equal(        // 4️⃣
  'HelloWorld:WORLD:1'
)
expect(() => HelloWorld.of('')).to.throw(                          // 5️⃣
  UnidentifiableEnumerationValueError
)
expect(() => HelloWorld.of(-42)).to.throw(
  UnidentifiableEnumerationValueError
)
```

1️⃣ Convert from an enum's `name` to its instance

2️⃣ Convert from an enum's `ordinal` to its instance

3️⃣ Default string form of an enum is its `name`

4️⃣ Fully qualified string includes colon-separated type, `name` & `ordinal`

5️⃣  `UnidentifiableEnumerationValueError` thrown on argument that can't be converted to an enum instance

> NOTE: There are helpful IDE-specific templates to assist you in following the patterns prescribed by this library.
> See ./src/templates for more information.
> If you don't see templates for your IDE, please submit a pull/merge request.

## Features

### `name()` instance method

This is an instance method that returns the instances `name`.

### `ordinal()` instance method

This is an instance method that returns the instance's `ordinal`.

### `toJSON()` instance method

This is an instance method to return the enum as an object suitable for JSON stringification. It simply returns
the `name` of this instance.

### `static values()` method

This is a static method on your enumeration class that returns all instances of the class.

### `static of()` method

This is a static method on your enumeration class that returns an instance of the class given its `name` or `ordinal`.

### Convenient sorting functions

This package exports two convenient sorting functions, `sortByName` & `sortByOrdinal` to sort enumeration instances by
their `name` or `ordinal`, respectively.

### Arbitrary additional properties & methods

Your enumeration class can define any additional properties or methods that you'd like.

Here's an example of a tri-state boolean (true, false or neither) with additional property `definite` and `toBoolean()`.
It also demonstrates the use of an incrementing `ordinal` to assign the ordinal values.

```typescript
import { _of, _values, Enumeration } from '@northscaler/better-enum'

let ordinal = -1 // convenient way to assign ordinals below

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
    return _values<Bool>(Bool)
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
   * Converts this {@link Bool} to a `boolean` if it's definite, else throws an `Error`.
   */
  toBoolean() {
    if (this === Bool.TRUE) return true
    if (this === Bool.FALSE) return false
    throw new Error('neither true nor false')
  }
}
```

Now, you can use your arbitrary properties:

```typescript
// mocha test examples

expect(Bool.TRUE.definite).to.be.true
expect(Bool.FALSE.definite).to.be.true
expect(Bool.NEITHER.definite).to.be.false
```

### Java's `Planet` enum

We ported Java's `Planet` enum example at https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html to this library.
It makes significant use of custom properties & methods.

See the [enum class](https://gitlab.com/northscaler-public/better-enum/-/blob/dev/src/test/unit/Planet.ts) and its [test](https://gitlab.com/northscaler-public/better-enum/-/blob/dev/src/test/unit/Enumeration.spec.ts#L123) if you're curious.
