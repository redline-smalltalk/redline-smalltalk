" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #CompilerTest.

- test
    self testEval.

- testEval
    self assert: (Compiler evaluate: '40 + 2') equals: 42 withMessage: 'evaluation not as expected.'.
