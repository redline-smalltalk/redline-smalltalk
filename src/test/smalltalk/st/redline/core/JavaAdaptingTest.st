" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

import: 'java.util.ArrayList' as: 'JList'.

TestCase < #JavaAdaptingTest.

- test
    self testCreateArrayList.
    
- testCreateArrayList
    |l|
    l := JList new.
    self assert: (l size) equals: 0 withMessage: 'JavaAdaptingTest testCreateArrayList failed'.
