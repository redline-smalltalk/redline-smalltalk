TestCase < #ObjectTest
  instanceVariableNames: 'object1 object2'.

- test
  object1 := Object new.
  object2 := Object new.
  self testIsKindOfObject.
  " self testIsKindOfArrayLiteral. FAILS, see issue #46 "
  self testIsMemberOfObject.
  self testIsMemberOfArrayLiteral.
  self testRespondsTo.
  self testClass.
  self testEquivalence.
  self testSimpleLiteralsEquivalence.
  self testSimpleEquals.
  " self testStringEquals. FAILS (not implemented yet in String). "
  self testNotEquals.
  self testNotEquivalent.
  self testHash.
  " self testIntegerHash. FAILS, see issue #60 "
  self testIsNil.
  self testNotNil.
  self testAt.
  self testAtPut.
  self testSize.
  self testYourself.
  " self testDoesNotUnderstand. Cannot really be tested yet -- need something like aBlock ifError:... "

- testIsKindOfObject
  self assertTrue: (object1 isKindOf: Object) withMessage: 'Object new should be isKindOf Object'.  

- testIsKindOfArrayLiteral
  self assertTrue: (#(this is an array) isKindOf: Collection) withMessage: 'array literal should be a Collection'.

- testIsMemberOfObject
  self assertTrue: (Object new isMemberOf: Object) withMessage: 'object should be member of Object'.
  self assertFalse: (Object new isMemberOf: Number) withMessage: 'object should not be a member of Number'.

- testIsMemberOfArrayLiteral
  self assertTrue:  (#(1 2 3) isMemberOf: Array) withMessage: 'array literal should be an Array'.
  self assertFalse: (#(this is an array) isMemberOf: Collection) withMessage: 'array literal should not be a member of Collection'.
  self assertFalse: (#(1 2 3) isMemberOf: Integer) withMessage: 'array literal should not be an Integer'.

- testRespondsTo
  self assertTrue: (Object new respondsTo: #isKindOf:) withMessage: 'object should #respondsTo: #isKindOf:'.
  self assertTrue: (3 respondsTo: #isKindOf:) withMessage: 'integer literal should respond to #isKindOf:'.
  self assertTrue: (ArrayedCollection includesSelector: #size) withMessage: 'includesSelector fail'.
  self assertTrue: (Integer includesSelector: #floor) withMessage: 'includesSelector fail'.
  self assertTrue: (3 respondsTo: #floor) withMessage: 'integer literal should respond to #floor'.
  self assertTrue: (3 respondsTo: #+) withMessage: 'integer literal should respond to #+'.
  self assertFalse: (object1 respondsTo: #xyz:) withMessage: 'test #respondsTo:'.
  self assertTrue: (Integer canUnderstand: #floor) withMessage: 'canUnderstand: failure'.
  self assertTrue: (Integer canUnderstand: #to:) withMessage: 'canUnderstand: failure'.

- testClass
  self assertTrue: (Object new class == Object) withMessage: 'object class should be Object'.
  self assertTrue: (#(this is an array) class == Array) withMessage: 'class of array literal should be Array'.
  self assertTrue: (3 class == Integer) withMessage: 'class of integer literal should be Integer'.
  self assertTrue: (#test class == Symbol) withMessage: 'class of symbol literal should be Symbol'.
  
- testEquivalence
  | object3 |
  self assertTrue: Object class == Object class withMessage: 'equivalence of class objects'.
  self assertTrue: object1 == object1 withMessage: 'object should == itself'.
  self assertFalse: object1 == object2 withMessage: 'different objects are not =='.
  object3 := object1.
  self assertTrue: object1 == object3 withMessage: 'two variables, same object'.
  self assertTrue: object3 == object1 withMessage: 'two variables, same object'.
  self assertTrue: 'a' == 'a' withMessage: 'string literals are not =='.

- testSimpleLiteralsEquivalence
  self assertTrue: 1 == 1 withMessage: 'integer literal equivalence'.
  self assertTrue: $a == $a withMessage: 'character literal equivalence'.
  self assertTrue: #a == #a withMessage: 'Symbol literal equivalence'. 

- testSimpleEquals
  self assert: object1 equals: object1 withMessage: 'same object equals'.
  self assertFalse: (object1 = object2) withMessage: 'different objects not equal'.

- testStringEquals
  "This should probably be in StringTest.st."
  self assert: 'a' equals:'a' withMessage: 'string literals should be equal'.

- testNotEquals
  self assertTrue: object1 ~= object2 withMessage: 'different objects should be not equal'.
  self assertFalse: object1 ~= object1 withMessage: 'same object should be not not equal'.
  
- testNotEquivalent
  self assertTrue: object1 ~~ object2 withMessage: 'different objects should be not equivalent'.
  self assertFalse: object1 ~~ object1 withMessage: 'same object should be not not equivalent'.

- testHash
  | object3 |
  object3 := object1.
  self assert: object1 hash equals: object1 hash withMessage: 'same object, hashes should be equal'.
  self assert: object1 hash equals: object3 hash withMessage: 'same object, hashes should be equal'.
  self assertTrue: object1 hash ~= object2 hash withMessage: 'different objects, different hashes'. 

- testIntegerHash
  self assert: 123 hash equals: 123 hash withMessage: 'integer literals should have same hash'. 
  self assert: 123 hash equals: 123 withMessage: 'integer literal should equals its hash?'. 

- testIsNil
  self assertTrue: nil isNil withMessage: 'nil should be isNil'.
  self assertFalse: object1 isNil withMessage: 'object should be not isNil'.

- testNotNil
  self assertTrue: object1 notNil withMessage: 'object should be notNil'.
  self assertFalse: nil notNil withMessage: 'nil should not be notNil'. 

- testAt
  | anArray anInt |
  anArray := #(1 2 3).
  self assert: (anArray at: 1) equals: 1 withMessage: 'test #at:'.
  self assert: (anArray at: 2) equals: 2 withMessage: 'test #at:'.
  self assert: (anArray basicAt: 1) equals: 1 withMessage: 'test #basicAt:'.
  self assert: (anArray basicAt: 2) equals: 2 withMessage: 'test #basicAt:'.
  "This fails as it should, but can't Assert test it yet: anArray at: 0. "
  "This fails as it should, but can't Assert test it yet: anArray at: 4. "
  "This gets a NPE, is that right? Object new at: 1. "
  anInt := 1.
  self assert: (anArray at: anInt) equals: 1 withMessage: 'test #at:'.
  self assert: (anArray basicAt: anInt) equals: 1 withMessage: 'test #basicAt:'.

- testAtPut
  | anArray |
  anArray := #(1 2 3).
  anArray at: 1 put: 3.
  self assert: (anArray at: 1) equals: 3 withMessage: 'test #at:put:'.
  anArray basicAt: 2 put: object1.
  self assert: (anArray at: 2) equals: object1 withMessage: 'test #basicAt:put:'.

- testSize
  | anArray |
  anArray := #(1 2 3).
  self assert: anArray size equals: 3 withMessage: 'size should be 3'.
  self assert: object1 size equals: 0 withMessage: 'non indexed size should be zero'.

- testYourself
  self assertTrue: object1 yourself == object1 withMessage: 'yourself should answer self'.

- testDoesNotUnderstand
  self shouldFail.

- testWIP
  "Not ready yet..."  
  object1 := WriteStream on: String new.
  object1 nextPutAll: 'test'.
  self assert: object1 printString equals: 'anObject' withMessage: 'testing #printString'.
