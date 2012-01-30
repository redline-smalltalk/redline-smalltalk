" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #NumberTest.

- test
    self testNegative.

- testNegative
    self assertTrue: -1 negative withMessage: '-1 is not negative'.
    self assertFalse: 0 negative withMessage: '0 is negative'.
    self assertFalse: 1 negative withMessage: '1 is negative'.