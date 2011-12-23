TestCase < #IntegerTest

- test
  self testSimpleEquality.
  self testSimpleAddition.
  self testComplexAddition.
  self testLessThan.
  self testGreaterThan.
  self testSimpleSubtraction.
  self testComplexSubtraction.
  self testLessThanOrEqualTo.
  self testGreaterThanOrEqualTo.
  self testPrecedence.

- testSimpleEquality
  Assertion assert: [ 1 = 1 ] withMessage: '1 is not 1.'
- testSimpleAddition
  Assertion assert: [ 1 + 1 = 2 ] withMessage: '1 and 1 do not make 2.'

- testComplexAddition
  Assertion assert: [ 1 + 2 + 3 + 4 = 10 ] withMessage: '1 + 2 + 3 + 4 is not as it should be.'.

- testLessThan
  Assertion assert: [ 1 < 2 ] withMessage: '1 is greater than 2.'

- testGreaterThan
  "Commented out due to parser error."

  "Assertion assert: [ 2 > 1 ] withMessage: '2 is less than 1.'"

- testSimpleSubtraction
  Assertion assert: [ 1 - 1 = 0 ] withMessage: '1 - 1 is not 0.'

- testComplexSubtraction  
  Assertion assert: [ 10 - 1 - 2 - 3 - 4 = 0] withMessage: '10 - 1 - 2 - 3 - 4 is not as it should be'

- testLessThanOrEqualTo
  Assertion assert: [ 1 <= 2 ] withMessage: '1 is not less than or equal to 2.'.
  Assertion assert: [ 2 <= 2 ] withMessage: '2 is not (less than or) equal to 2.'.  

- testGreaterThanOrEqualTo
  Assertion assert: [ 2 >= 1 ] withMessage: '2 is not greater than (or equal to) 1.'.
  Assertion assert: [ 2 >= 2 ] withMessage: '2 is not (greater than or) equal to 2.'.

- testPrecedence
  "Commented out til we have * support on Integers"

  "Assertion assert: [ (25 + 3 * 4) = 112 ] withMessage: 'Mathmatical precedence is wrong.'."
      