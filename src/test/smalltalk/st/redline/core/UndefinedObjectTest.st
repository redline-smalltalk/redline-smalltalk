" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #UndefinedObjectTest.
  
- test
    self testIsNil.
    self testNotNil.

- testIsNil
    self assertTrue: nil isNil withMessage: 'isNil failure'.

- testNotNil
    self assertFalse: nil notNil withMessage: 'notNil failure'.
