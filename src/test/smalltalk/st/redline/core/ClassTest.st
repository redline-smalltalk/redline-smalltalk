" Copyright (c) 2012 Redline Smalltalk. All rights reserved. See LICENSE in the root of this distribution "

TestCase < #ClassTest.

- test
    self testSubclass.

- testSubclass
    | class |

    class := Class subclass: #Foo.
    self assertTrue: (class fullyQualifiedName = 'st.redline.core.Foo') withMessage: 'subclass message selector failure'.