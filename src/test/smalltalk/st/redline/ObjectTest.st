TestCase < #ObjectTest
  instanceVariableNames: 'object1 object2'.

- test
  object1 := Object new.
  object2 := Object new.
  self testIsKindOfObject.
  " self testIsKindOfArrayLiteral. FAILS, see issue #46 "
  self testIsMemberOfObject.
  self testIsMemberOfArrayLiteral.
  " self testRespondsTo. FAILS, see issue #48 "
  self testClass.
  self testEquivalence.
  " self testSimpleLiteralsEquivalence. FAILS, see issue #50. "
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
  " self testSize. FAILS, see issue #61. "
  self testYourself.

- testIsKindOfObject
  Assertion assert: [ object1 isKindOf: Object ] withMessage: 'Object new should be isKindOf Object'.  

- testIsKindOfArrayLiteral
  Assertion assert: [ #(this is an array) isKindOf: Collection ] withMessage: 'array literal should be a Collection'.

- testIsMemberOfObject
  Assertion assert: [ Object new isMemberOf: Object ] withMessage: 'object should be member of Object'.
  Assertion assert: [ (Object new isMemberOf: Number) not ] withMessage: 'object should not be a member of Number'.

- testIsMemberOfArrayLiteral
  Assertion assert: [ #(1 2 3) isMemberOf: Array ] withMessage: 'array literal should be an Array'.
  Assertion assert: [ (#(this is an array) isMemberOf: Collection) not ] withMessage: 'array literal should not be a member of Collection'.
  Assertion assert: [ (#(1 2 3) isMemberOf: Integer) not ] withMessage: 'array literal should not be an Integer'.

- testRespondsTo
  Assertion assert: [ Object new respondsTo: #isKindOf: ] withMessage: 'object should #respondsTo: #isKindOf:'.
  Assertion assert: [ 3 respondsTo: #isKindOf: ] withMessage: 'integer literal should respond to #isKindOf:'.
  Assertion assert: [ (object1 respondsTo: #xyz:) not ] withMessage: 'test #respondsTo:'.

- testClass
  Assertion assert: [ Object new class == Object ] withMessage: 'object class should be Object'.
  Assertion assert: [ #(this is an array) class == Array ] withMessage: 'class of array literal should be Array'.
  Assertion assert: [ 3 class == Integer ] withMessage: 'class of integer literal should be Integer'.
  Assertion assert: [ #test class == Symbol ] withMessage: 'class of symbol literal should be Symbol'.
  
- testEquivalence
  | object3 |
  "FAILS, issue #49: Assertion assert: [ Object class == Object class ] withMessage: 'equivalence of class objects'."
  Assertion assert: [ object1 == object1 ] withMessage: 'object should == itself'.
  Assertion assert: [ (object1 == object2) not ] withMessage: 'different objects are not =='.
  object3 := object1.
  Assertion assert: [ object1 == object3 ] withMessage: 'two variables, same object'.
  Assertion assert: [ object3 == object1 ] withMessage: 'two variables, same object'.
  Assertion assert: [ ('a' == 'a') not ] withMessage: 'string literals are not =='.

- testSimpleLiteralsEquivalence
  Assertion assert: [ 1 == 1 ] withMessage: 'integer literal equivalence'.
  Assertion assert: [ $a == $a ] withMessage: 'character literal equivalence'.
  Assertion assert: [ #a == #a ] withMessage: 'Symbol literal equivalence'. 

- testSimpleEquals
  Assertion assert: [ object1 = object1 ] withMessage: 'same object equals'.
  Assertion assert: [ (object1 = object2) not ] withMessage: 'different objects not equal'.

- testStringEquals
  "This should probably be in StringTest.st."
  Assertion assert: [ 'a' = 'a' ] withMessage: 'string literals should be equal'.

- testNotEquals
  Assertion assert: [ object1 ~= object2 ] withMessage: 'different objects should be not equal'.
  Assertion assert: [ (object1 ~= object1) not ] withMessage: 'same object should be not not equal'.
  
- testNotEquivalent
  Assertion assert: [ object1 ~~ object2 ] withMessage: 'different objects should be not equivalent'.
  Assertion assert: [ (object1 ~~ object1) not ] withMessage: 'same object should be not not equivalent'.

- testHash
  | object3 |
  object3 := object1.
  Assertion assert: [ object1 hash = object1 hash ] withMessage: 'same object, hashes should be equal'.
  Assertion assert: [ object1 hash = object3 hash ] withMessage: 'same object, hashes should be equal'.
  Assertion assert: [ object1 hash ~= object2 hash ] withMessage: 'different objects, different hashes'. 

- testIntegerHash
  Assertion assert: [ 123 hash = 123 hash ] withMessage: 'integer literals should have same hash'. 
  Assertion assert: [ 123 hash = 123 ] withMessage: 'integer literal should equals its hash?'. 

- testIsNil
  Assertion assert: [ nil isNil ] withMessage: 'nil should be isNil'.
  Assertion assert: [ object1 isNil not ] withMessage: 'object should be not isNil'.

- testNotNil
  Assertion assert: [ object1 notNil ] withMessage: 'object should be notNil'.
  Assertion assert: [ nil notNil not ] withMessage: 'nil should not be notNil'. 

- testAt
  | anArray anInt |
  anArray := #(1 2 3).
  Assertion assert: [ (anArray at: 1) = 1 ] withMessage: 'test #at:'.
  Assertion assert: [ (anArray at: 2) = 2 ] withMessage: 'test #at:'.
  Assertion assert: [ (anArray basicAt: 1) = 1 ] withMessage: 'test #basicAt:'.
  Assertion assert: [ (anArray basicAt: 2) = 2 ] withMessage: 'test #basicAt:'.
  "This fails as it should, but can't Assert test it yet: anArray at: 0. "
  "This fails as it should, but can't Assert test it yet: anArray at: 4. "
  "This gets a NPE, is that right? Object new at: 1. "
  anInt := 1.
  Assertion assert: [ (anArray at: anInt) = 1 ] withMessage: 'test #at:'.
  Assertion assert: [ (anArray basicAt: anInt) = 1 ] withMessage: 'test #basicAt:'.

- testAtPut
  | anArray |
  anArray := #(1 2 3).
  anArray at: 1 put: 3.
  Assertion assert: [ (anArray at: 1) = 3 ] withMessage: 'test #at:put:'.
  anArray basicAt: 2 put: object1.
  Assertion assert: [ (anArray at: 2) = object1 ] withMessage: 'test #basicAt:put:'.

- testSize
  | anArray |
  anArray := #(1 2 3).
  Assertion assert: [ anArray size = 3 ] withMessage: 'size should be 3'.
  Assertion assert: [ object1 size = 0 ] withMessage: 'non indexed size should be zero'.

- testYourself
  Assertion assert: [ object1 yourself == object1 ] withMessage: 'yourself should answer self'.


- testWIP
  "Not ready yet..."  
  object1 := WriteStream on: String new.
  object1 nextPutAll: 'test'.
  Assertion assert: [ object1 printString = 'anObject' ] withMessage: 'testing #printString'.







