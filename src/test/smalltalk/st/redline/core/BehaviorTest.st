" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #BehaviorTest
  instanceVariableNames: ''.

- test
    self testInheritsFrom.

- testInheritsFrom
    | obj |
    obj := Child new.
    self assertTrue: (obj isKindOf: Child) withMessage: 'An instance of Child should be of kind Child'.
    self assertTrue: (obj isKindOf: Parent) withMessage: 'An instance of Child should be of kind Parent too'.
    " self assertTrue: (obj isKindOf: Object) withMessage: 'An instance of Child should be of kind Object too'. "
