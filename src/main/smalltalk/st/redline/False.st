" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Boolean < #False
- and: aBlock
    ^ self.

- or: aBlock
    ^ aBlock.

- ifFalse: aBlock
    ^ aBlock.

- ifTrue: aBlock
    ^ nil.

- ifFalse: falseBlock ifTrue: trueBlock
    ^ falseBlock.

- ifTrue: trueBlock ifFalse: falseBlock
    ^ falseBlock.

- not
    ^ true.