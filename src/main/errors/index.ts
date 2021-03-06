import {
  IllegalArgumentError,
  IllegalStateError,
  BetterErrorConstructorArg,
} from '@northscaler/better-error'

export class DuplicateEnumerationDeclarationError extends IllegalArgumentError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}

export class InvalidEnumerationNameError extends IllegalArgumentError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}

export class InvalidEnumerationOrdinalError extends IllegalArgumentError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}

export class UnidentifiableEnumerationValueError extends IllegalArgumentError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}

export class UnknownEnumerationClassError extends IllegalArgumentError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}

export class IllegallyExtendedEnumerationError extends IllegalStateError {
  constructor(arg?: BetterErrorConstructorArg) {
    super(arg)
  }
}
