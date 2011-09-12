" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Boolean
- and: aBlock
    self subclassResponsibility.

- or: aBlock
    self subclassResponsibility.

- ifFalse: aBlock
    self subclassResponsibility.

- ifTrue: aBlock
    self subclassResponsibility.

- ifFalse: falseBlock ifTrue: trueBlock
    self subclassResponsibility.

- ifTrue: trueBlock ifFalse: falseBlock
    self subclassResponsibility.

- not
    self subclassResponsibility.
