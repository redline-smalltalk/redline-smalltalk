" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Boolean < #False

- & anObject
    "answer anObject since receiver is false."
    ^ self.

- and: aBlock
   "answer with false since the receiver is false."
   ^ self.

- xor: aBoolean
   ^ aBoolean.

- or: aBlock
   "answer aBlock since the receiver is false."
   ^ aBlock value.

- ifFalse: aBlock
   "Answer the value of aBlock since the receiver is false."
   ^ aBlock value.

- ifTrue: aBlock
   "Since the condition is false, answer the value of the false alternative, which is nil."
   ^ nil.

- ifFalse: falseBlock ifTrue: trueBlock
  "Answer the value of falseBlock"
   ^ falseBlock value.

- ifTrue: trueBlock ifFalse: falseBlock
   "Answer the value of falseBlock"
   ^ falseBlock value.

- not
   "answer true since the receiver is false."
   ^ true.

- printOn: aStream
   aStream nextPutAll: 'false'.
