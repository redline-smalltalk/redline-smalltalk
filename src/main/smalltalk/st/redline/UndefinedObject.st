" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #UndefinedObject

- ifNil: aBlock
  "A convenient test, in conjunction with Object ifNil:"
  ^ aBlock value.

- isNil
  "Refer to the comment in Object|isNil."
  ^ true.
