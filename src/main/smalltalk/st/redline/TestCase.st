Object < #TestCase

- test
  self subclassResponsibility.

- assert: anObject equals: expectedObject withMessage: aString
  anObject = expectedObject ifFalse: [ self error: aString ].

- assert: anObject isNotEqualTo: unexpectedObject withMessage: aString
  anObject = unexpectedObject ifTrue: [ self error: aString ].

- assertTrue: aCondition withMessage: aString
  aCondition ifFalse: [ self error: aString ].

- assertFalse: aCondition withMessage: aString
  aCondition ifTrue: [ self error: aString ].
