" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Boolean < #True

- & anObject
    "answer anObject since receiver is true."
	^ anObject.

- and: aBlock
    "answer the value of aBlock since the receiver is true."
    ^ aBlock.

- xor: aBoolean
    ^ aBoolean not.

- or: aBlock
    "answer true since the receiver is true."
    ^ self.

- ifFalse: aBlock
    "Since the condition is true, the value is the true alternative, which is nil."
    ^ nil.

- ifTrue: aBlock
    "Answer the value of aBlock since the receiver is true."
    ^ aBlock value.

- ifFalse: falseBlock ifTrue: trueBlock
   "Answer the value of trueBlock"
    ^ trueBlock value.

- ifTrue: trueBlock ifFalse: falseBlock
    "Answer the value of trueBlock"
    ^ trueBlock value.

- not
    "answer false since the receiver is true."
    ^ false.

- printOn: aStream
    aStream nextPutAll: 'true'.
