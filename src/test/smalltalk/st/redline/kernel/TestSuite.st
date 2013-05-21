"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #TestSuite.

- run
    self show: 'Running UndefinedObjectTests'.
    UndefinedObjectTest new test.
    self show: 'Running TrueTests'.
    TrueTest new test.
    self show: 'Running FalseTests'.
    FalseTest new test.
    self show: 'Running ObjectTests'.
    ObjectTest new test.

- show: message
    Transcript show: message; cr.
