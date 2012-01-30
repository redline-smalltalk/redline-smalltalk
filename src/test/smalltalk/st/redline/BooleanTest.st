" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #BooleanTest
  instanceVariableNames: 'aTrue aFalse'.
  
- test
    aTrue := 1 = 1.
    aFalse := 1 = 2.
    self testAmpersand.
    self testEqv.
    self testNot.
    self testXor.
    self testAnd.
    self testIfFalse.
    self testIfFalseIfTrue.
    self testIfTrue.
    self testIfTrueIfFalse.
    self testOr.

- testAmpersand
    self assertTrue: true & true withMessage: 'boolean ampersand failure'.
    self assertFalse: true & false withMessage: 'boolean ampersand failure'.
    self assertFalse: false & true withMessage: 'boolean ampersand failure'.
    self assertFalse: false & false withMessage: 'boolean ampersand failure'.
  
- testEqv
    self assertTrue: (true eqv: true) withMessage: 'boolean eqv failure'.
    self assertFalse: (true eqv: false) withMessage: 'boolean eqv failure'.
    self assertFalse: (false eqv: true) withMessage: 'boolean eqv failure'.
    self assertTrue: (false eqv: false) withMessage: 'boolean eqv failure'.
    self assertTrue: (aTrue eqv: aTrue) withMessage: 'boolean eqv failure'.
    self assertFalse: (aTrue eqv: aFalse) withMessage: 'boolean eqv failure'.
    self assertFalse: (aFalse eqv: aTrue) withMessage: 'boolean eqv failure'.
    self assertTrue: (aFalse eqv: aFalse) withMessage: 'boolean eqv failure'.

- testNot
    self assertTrue: false not withMessage: 'boolean not failure'.
    self assertTrue: aFalse not withMessage: 'boolean not failure'.
    self assertFalse: true not withMessage: 'boolean not failure'.
    self assertFalse: aTrue not withMessage: 'boolean not failure'.
  
- testXor
    self assertFalse: (true xor: true) withMessage: 'boolean xor failure'.
    self assertTrue: (true xor: false) withMessage: 'boolean xor failure'.
    self assertTrue: (false xor: true) withMessage: 'boolean xor failure'.
    self assertFalse: (false xor: false) withMessage: 'boolean xor failure'.
  
- testAnd
    | aBlock evaluated |
    self assertTrue: (true and: true) withMessage: 'boolean and failure'.
    self assertFalse: (true and: false) withMessage: 'boolean and failure'.
    self assertFalse: (false and: true) withMessage: 'boolean and failure'.
    self assertFalse: (false and: false) withMessage: 'boolean and failure'.
    aBlock := [evaluated := true].
    evaluated := false.
    self assertTrue: (true and: aBlock) withMessage: 'boolean and failure'.
    self assertTrue: evaluated withMessage: 'boolean and failure'.
    evaluated := false.
    self assertFalse: (false and: aBlock) withMessage: 'boolean and failure'.
    self assertFalse: evaluated withMessage: 'boolean and failure'.
  
- testIfFalse
    | evaluated |
    true ifFalse: [ self fail: 'failure in ifFalse:' ].
    aTrue ifFalse: [ self fail: 'failure in ifFalse:' ].
    1 = 1 ifFalse: [ self fail: 'failure in ifFalse:' ].
    evaluated := false.
    false ifFalse: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse: failure'.
    evaluated := false.
    aFalse ifFalse: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse: failure'.
    evaluated := false.
    1 = 2 ifFalse: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse: failure'.

- testIfFalseIfTrue
    | evaluated |
    true ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    aTrue ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    (1 = 1) ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    false ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertFalse: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    aFalse ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertFalse: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    (1 = 2) ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ].
    self assertFalse: evaluated withMessage: 'ifFalse:ifTrue: failure'.
    self assertTrue: (true ifFalse: [ false ] ifTrue: [ true ]) withMessage: 'ifFalse:ifTrue: failure'.
    self assertTrue: (true ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true]) withMessage: 'ifFalse:ifTrue: failure'.
    self assertFalse: (false ifFalse: [ false ] ifTrue: [ true ]) withMessage: 'ifFalse:ifTrue: failure'.
    self assertFalse: (false ifFalse: [ evaluated := false ] ifTrue: [ evaluated := true ]) withMessage: 'ifFalse:ifTrue: failure'.
        
- testIfTrue
    | evaluated |
    evaluated := false.
    true ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifTrue: failure'.
    evaluated := false.
    aTrue ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifTrue: failure'.
    evaluated := false.
    1 = 1 ifTrue: [ evaluated := true ].
    self assertTrue: evaluated withMessage: 'ifTrue: failure'.
    false ifTrue: [ self fail: 'failure in ifTrue:' ].
    aFalse ifTrue: [ self fail: 'failure in ifTrue:' ].
    1 = 2 ifTrue: [ self fail: 'failure in ifTrue:' ].

- testIfTrueIfFalse
    | evaluated |
    true ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertTrue: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    aTrue ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertTrue: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    (1 = 1) ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertTrue: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    false ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertFalse: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    aFalse ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertFalse: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    (1 = 2) ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ].
    self assertFalse: evaluated withMessage: 'ifTrue:ifFalse: failure'.
    self assertTrue: (true ifTrue: [ true ] ifFalse: [ false ]) withMessage: 'ifTrue:ifFalse: failure'.
    self assertTrue: (true ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ]) withMessage: 'ifTrue:ifFalse: failure'.
    self assertFalse: (false ifTrue: [ true ] ifFalse: [ false ]) withMessage: 'ifTrue:ifFalse: failure'.
    self assertFalse: (false ifTrue: [ evaluated := true ] ifFalse: [ evaluated := false ]) withMessage: 'ifTrue:ifFalse: failure'.

- testOr
    | aBlock evaluated |
    self assertTrue: (true or: true) withMessage: 'boolean or failure'.
    self assertTrue: (true or: false) withMessage: 'boolean or failure'.
    self assertTrue: (false or: true) withMessage: 'boolean or failure'.
    self assertFalse: (false or: false) withMessage: 'boolean or failure'.
    aBlock := [evaluated := true].
    evaluated := false.
    self assertTrue: (true or: aBlock) withMessage: 'boolean or failure'.
    self assertFalse: evaluated withMessage: 'boolean or failure'.
    evaluated := false.
    self assertTrue: (false or: aBlock) withMessage: 'boolean or failure'.
    self assertTrue: evaluated withMessage: 'boolean or failure'.
