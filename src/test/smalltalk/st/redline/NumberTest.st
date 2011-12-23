TestCase < #NumberTest

- test
  self testNegative.

- testNegative
  Assertion assert: [ -1 negative = true ] withMessage: '-1 is not negative'.
  Assertion assert: [ 0 negative = false ] withMessage: '0 is negative'.
  Assertion assert: [ 1 negative = false ] withMessage: '1 is negative'.