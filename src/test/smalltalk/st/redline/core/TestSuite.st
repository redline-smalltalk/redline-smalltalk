" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #TestSuite.

- run
    Transcript show: 'Running ArrayTest Suite'; cr.
    ArrayTest new test.
    Transcript show: 'Running BooleanTest Suite'; cr.
    BooleanTest new test.
    Transcript show: 'Running UndefinedObjectTest Suite'; cr.
    UndefinedObjectTest new test.
    Transcript show: 'Running IntegerTest Suite'; cr.
    IntegerTest new test.
    Transcript show: 'Running NumberTest Suite'; cr.
    NumberTest new test.
    Transcript show: 'Running ObjectTest Suite'; cr.
    ObjectTest new test.
    Transcript show: 'Running BehaviorTest Suite'; cr.
    BehaviorTest new test.
    Transcript show: 'Running ClassTest Suite'; cr.
    ClassTest new test.
    Transcript show: 'Running CompilerTest Suite'; cr.
    CompilerTest new test.
    Transcript show: 'Running JavaAdaptingTest Suite'; cr.
    JavaAdaptingTest new test.
