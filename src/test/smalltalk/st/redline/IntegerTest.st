" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #IntegerTest.

- test
    self testSimpleEquality.
    self testSimpleAddition.
    self testComplexAddition.
    self testLessThan.
    self testGreaterThan.
    self testSimpleSubtraction.
    self testComplexSubtraction.
    self testSimpleMultiplication.
    self testComplexMultiplication.
    self testSimpleDivision.
    self testLessThanOrEqualTo.
    self testGreaterThanOrEqualTo.
    self testPrecedence.
    self testFloor.
    self testTruncated.
    self testIsInteger.

- testSimpleEquality
    self assert: 1 equals: 1withMessage: '1 is not 1.'

- testSimpleAddition
    self assert: 1 + 1 equals: 2 withMessage: '1 and 1 do not make 2.'

- testComplexAddition
    self assert: 1 + 2 + 3 + 4 equals: 10 withMessage: '1 + 2 + 3 + 4 is not as it should be.'.

- testLessThan
    self assertTrue: 1 < 2 withMessage: '1 is greater than 2.'

- testGreaterThan
    self assertTrue: 2 > 1 withMessage: '2 is less than 1.'

- testSimpleSubtraction
    self assert: 1 - 1 equals: 0 withMessage: '1 - 1 is not 0.'

- testComplexSubtraction  
    self assert: 10 - 1 - 2 - 3 - 4 equals: 0 withMessage: '10 - 1 - 2 - 3 - 4 is not as it should be'

- testLessThanOrEqualTo
    self assertTrue: 1 <= 2 withMessage: '1 is not less than or equal to 2.'.
    self assertTrue: 2 <= 2 withMessage: '2 is not (less than or) equal to 2.'.

- testGreaterThanOrEqualTo
    self assertTrue: 2 >= 1 withMessage: '2 is not greater than (or equal to) 1.'.
    self assertTrue: 2 >= 2 withMessage: '2 is not (greater than or) equal to 2.'.

- testSimpleMultiplication
    self assert: 1 * 2 equals: 2 withMessage: '1 times 2 is not 2.'.

- testComplexMultiplication
    self assert: 1 * 2 * 3 equals: 6 withMessage: '1 times 2 times 3 is not 6.'.

- testSimpleDivision
    self assert: 10 / 5 equals: 2 withMessage: '10 divided by 5 is not 2.'.

- testPrecedence
    self assert: 25 + 3 * 4 equals: 112 withMessage: 'Mathmatical precedence is wrong.'.

- testFloor
    self assert: 1 floor equals: 1 withMessage: '1 floored is not 1.'.

- testTruncated
    self assert: 1 truncated equals: 1 withMessage: '1 truncated is not 1.'.

- testIsInteger
    self assertTrue: 1 isInteger withMessage: '1 is not an Integer'.
