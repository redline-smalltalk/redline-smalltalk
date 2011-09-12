" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Boolean < #True
- and: alternativeBlock
    ^ alternativeBlock.

- or: aBlock
    ^ self.

- ifFalse: aBlock
    ^ nil.

- ifTrue: aBlock
    ^ aBlock.

- ifFalse: falseBlock ifTrue: trueBlock
    ^ trueBlock.

- ifTrue: trueBlock ifFalse: falseBlock
    ^ trueBlock.

- not
    ^ false.