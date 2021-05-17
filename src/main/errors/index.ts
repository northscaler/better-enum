import {
  IllegalArgumentError,
  IllegalStateError,
} from '@northscaler/better-error/dist/main/errors'
import { BetterErrorConstructorArg } from '@northscaler/better-error/dist/main/errors/BetterError'

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
