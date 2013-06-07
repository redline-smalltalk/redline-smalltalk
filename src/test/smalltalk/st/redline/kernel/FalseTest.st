"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

self import: 'st.redline.test.TestCase'.

TestCase subclass: #FalseTest.

- test
    self testFalse.
    self testIfTrue.
    self testIfFalse.
    self testLogicalOperations.
    self testControllingMethods.

- testFalse
    self assertFalse: false withMessage: 'should be false.'.

- testIfTrue
    "Assert False answers nil in response to ifTrue."
    self assert: (false ifTrue: [ true ]) equals: nil withMessage: 'should answer nil.'.

- testIfFalse
    "Assert False answers [] value response to ifFalse."
    self assert: (false ifFalse: [ true ]) equals: true withMessage: 'should answer nil.'.

- testLogicalOperations
    "Assert False has expected logical operations."
    self assertTrue: (false not) withMessage: 'should answer true.'.
    self assertFalse: (false & nil) withMessage: 'should answer false.'.
    self assert: (false | nil) equals: nil withMessage: 'should answer nil.'.
    self assertTrue: (false eqv: false) withMessage: 'should answer true.'.
    self assertTrue: (false xor: true) withMessage: 'should answer true.'.

- testControllingMethods
    "Assert False supports each of the controlling methods."
    self assert: (false ifTrue: [ nil ] ifFalse: [ true ]) equals: true withMessage: 'should answer true.'.
    self assert: (false ifFalse: [ true ] ifTrue: [ nil ]) equals: true withMessage: 'should answer true.'.
    self assertFalse: (false and: [ true ]) withMessage: 'should answer false.'.
    self assertTrue: (false or: [ true ]) withMessage: 'should answer true.'.
