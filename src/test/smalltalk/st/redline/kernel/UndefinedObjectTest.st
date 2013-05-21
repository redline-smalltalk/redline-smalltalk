"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

self import: 'st.redline.test.TestCase'.

TestCase subclass: #UndefinedObjectTest.

- test
    self testTestingMethods.
    self testCopyingMethods.

- testTestingMethods
    "Assert UndefinedObject supports each of the testing methods."
    self assertTrue: (nil isNil) withMessage: 'should be true.'.
    self assertFalse: (nil notNil) withMessage: 'should be false.'.
    self assert: (nil ifNotNil: [ true ]) equals: nil withMessage: 'should be nil.'.
    self assertTrue: (nil ifNil: [ true ]) withMessage: 'should be true.'.

- testCopyingMethods
    "Assert UndefinedObject supports each of the copying methods."
    self assert: (nil deepCopy) equals: nil withMessage: 'should be nil.'.
    self assert: (nil shallowCopy) equals: nil withMessage: 'should be nil.'.
