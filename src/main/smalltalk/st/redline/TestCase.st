Object < #TestCase

- test
  self subclassResponsibility.

- assert: anObject equals: expectedObject withMessage: aString
  anObject = expectedObject ifFalse: [ Object error: aString ].

- assert: anObject isNotEqualTo: unexpectedObject withMessage: aString
  anObject = unexpectedObject ifTrue: [ Object error: aString ].

- assertTrue: aCondition withMessage: aString
  aCondition ifFalse: [ Object error: aString ].

- assertFalse: aCondition withMessage: aString
  aCondition ifTrue: [ Object error: aString ].
