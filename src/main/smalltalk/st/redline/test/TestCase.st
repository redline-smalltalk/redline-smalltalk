"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #TestCase.

"-- running"

- test
    self subclassResponsibility.

"-- asserting"

- assert: anObject equals: expectedObject withMessage: aString
    anObject = expectedObject ifFalse: [ self error: aString ].

- assert: anObject isNotEqualTo: unexpectedObject withMessage: aString
    anObject = unexpectedObject ifTrue: [ self error: aString ].

- assertTrue: aCondition withMessage: aString
    aCondition ifFalse: [ self error: aString ].

- assertFalse: aCondition withMessage: aString
    aCondition ifTrue: [ self error: aString ].

- fail: aString
    self error: aString.
