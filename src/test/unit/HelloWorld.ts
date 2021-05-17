import { _of, _values, Enumeration } from '../../main'

export default class HelloWorld extends Enumeration<HelloWorld> {
  static readonly HELLO = new HelloWorld('HELLO', 0)
  static readonly WORLD = new HelloWorld('WORLD', 1)

  static of(it: HelloWorld | string | number): HelloWorld {
    return _of(it, HelloWorld)
  }

  static values(): HelloWorld[] {
    return _values<HelloWorld>(HelloWorld)
  }

  private constructor(name: string, ordinal: number) {
    super(name, ordinal, HelloWorld)
  }
}
