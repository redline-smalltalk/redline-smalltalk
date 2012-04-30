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
    | array |

    "How do we test this?"
    array := #(1 2 3 4 5).
    array do: [ :each |  ].

