| object1 object2 object3 array1 anInt |
object1 := Object new.
object2 := Object new.

Assert assert: [ object1 isKindOf: Object ] withMessage: 'test #isKindOf:'.  

" produces a TODO doesNotUnderstand (issue #46) 
Assert assert: [ #(this is an array) isKindOf: Collection ] withMessage: 'test #isKindOf:'. " 

Assert assert: [ object1 isMemberOf: Object ] withMessage: 'test #isMemberOf:'.
Assert assert: [ (object1 isMemberOf: Number) not ] withMessage: 'test #isMemberOf:'.
" Fails: (issue #47) 
Assert assert: [ #(this is an array) isMemberOf: Collection ] withMessage: 'test #isMemberOf:'. "
Assert assert: [ #(1 2 3) isMemberOf: Array ] withMessage: 'test #isMemberOf:'.

" produces TODO exception (issue #48)
Assert assert: [ object1 respondsTo: #isKindOf: ] withMessage: 'test #respondsTo:'.
Assert assert: [ 3 respondsTo: #isKindOf: ] withMessage: 'test #respondsTo:'.
Assert assert: [ (object1 respondsTo: #xyz:) not ] withMessage: 'test #respondsTo:'.  "

Assert assert: [ object1 class == Object ] withMessage: 'test #class'.
" Fails: (issue #49)
Assert assert: [ Object class == Object class ] withMessage: 'test #class'. "
Assert assert: [ #(this is an array) class == Array ] withMessage: 'test #class'.
Assert assert: [ 3 class == Integer ] withMessage: 'test class'.

Assert assert: [ object1 == object1 ] withMessage: 'test equality'.
Assert assert: [ (object1 == object2) not ] withMessage: 'test equality'.
object3 := object1.
Assert assert: [ object1 == object3 ] withMessage: 'test equality'.
Assert assert: [ object3 == object1 ] withMessage: 'test equality'.
" Fails: issue #50
object1 := 1. object2 := 1.
Assert assert: [ object1 == object2 ] withMessage: 'test equality'.
Assert assert: [ 1 == 1 ] withMessage: 'test equality'. 
Assert assert: [ $a == $a ] withMessage: 'test equality'.
Assert assert: [ #a == #a ] withMessage: 'test equality'. "
Assert assert: [ ('a' == 'a') not ] withMessage: 'test equality'.

Assert assert: [ object1 = object1 ] withMessage: 'test equal'.
Assert assert: [ (object1 = object2) not ] withMessage: 'test equal'.

Assert assert: [ object1 ~= object2 ] withMessage: 'test not equal'.
Assert assert: [ (object1 ~= object1) not ] withMessage: 'test not equal'.

Assert assert: [ object1 ~~ object2 ] withMessage: 'test not equality'.
Assert assert: [ (object1 ~~ object1) not ] withMessage: 'test not equality'.

Assert assert: [ object1 hash = object1 hash ] withMessage: 'test #hash'.
Assert assert: [ object1 hash = object3 hash ] withMessage: 'test #hash'.
Assert assert: [ object1 hash ~= object2 hash ] withMessage: 'test #hash'. 
"Fails, is it a bug?
Assert assert: [ 123 hash = 123 hash ] withMessage: 'test #hash'. "

Assert assert: [ nil isNil ] withMessage: 'test #isNil'.
Assert assert: [ object1 isNil not ] withMessage: 'test #isNil'.

Assert assert: [ object1 notNil ] withMessage: 'test #notNil'.
Assert assert: [ nil notNil not ] withMessage: 'test #notNil'. 

array1 := #(1 2 3).
Assert assert: [ (array1 at: 1) = 1 ] withMessage: 'test #at:'.
Assert assert: [ (array1 at: 2) = 2 ] withMessage: 'test #at:'.
Assert assert: [ (array1 basicAt: 1) = 1 ] withMessage: 'test #basicAt:'.
Assert assert: [ (array1 basicAt: 2) = 2 ] withMessage: 'test #basicAt:'.
"This fails as it should, but can't Assert test it yet: array1 at: 0. "
"This fails as it should, but can't Assert test it yet: array1 at: 4. "
"This gets a NPE, is that right? Object new at: 1. "
anInt := 1.
Assert assert: [ (array1 at: anInt) = 1 ] withMessage: 'test #at:'.
Assert assert: [ (array1 basicAt: anInt) = 1 ] withMessage: 'test #basicAt:'.

array1 at: 1 put: 3.
Assert assert: [ (array1 at: 1) = 3 ] withMessage: 'test #at:put:'.
array1 basicAt: 2 put: object1.
Assert assert: [ (array1 at: 2) = object1 ] withMessage: 'test #basicAt:put:'.

"Fail - issue #61
Transcript show: array1 basicSize;cr.
Transcript show: object1 basicSize;cr.
Assert assert: [ array1 size = 3 ] withMessage: 'test #size'.
Assert assert: [ object1 size = 1 ] withMessage: 'test #size'. "





