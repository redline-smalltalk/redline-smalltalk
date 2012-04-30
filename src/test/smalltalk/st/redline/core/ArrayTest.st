" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #ArrayTest.

- test
    self testAt.
    self testDo.

- testAt
    | array element |

    array := #(1 'one' 17 symbol $f 3).
    element := array at: 3.
 
    self assert: element equals: 17 withMessage: 'Array test failed'.

- testDo
    | array lastElement passes |

    "How do we test this?"
    array := #(1 2 3 4 5).
    lastElement := 1.
    passes := 0.
    array do: [ :each |  
      self assertTrue: (each == lastElement) withMessage: 'should be equal'.
      lastElement := each + 1.
      passes := passes + 1.
    ].

    self assert: passes equals: 5 withMessage: 'should be 5 items iterated over in the array.'.


