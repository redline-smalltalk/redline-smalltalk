"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Boolean subclass: #True.

"category: controlling"

- ifFalse: alternativeBlock
    "Since the condition is true, the value is the true alternative, which is nil."
    ^ nil.

- ifTrue: alternativeBlock
    "Answer the value of alternativeBlock."
    ^ alternativeBlock value.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
    "Answer with the value of trueAlternativeBlock."
    ^ trueAlternativeBlock value.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
    "Answer the value of trueAlternativeBlock."
    ^ trueAlternativeBlock value.

- or: alternativeBlock
    "Nonevaluating disjunction -- answer true since the receiver is true."
    ^ self.

- and: alternativeBlock
    "Nonevaluating conjunction -- answer the value of alternativeBlock since the receiver is true."
    ^ alternativeBlock value.

"category: logical operations"

- & alternativeObject
    "Evaluating conjunction -- answer alternativeObject since receiver is true."
    ^ alternativeObject.

- not
    "Negation -- answer false since the receiver is true."
    ^ false.

- | aBoolean
    "Evaluating disjunction (OR) -- answer with the argument, aBoolean."
    ^ self.

- eqv: aBoolean
    "Answer true if the receiver is equivalent to the argument, aBoolean."
    ^ self == aBoolean.

- xor: aBoolean
    "Exclusive OR. Answer true if the receiver is not equivalent to aBoolean."
    ^ self ~= aBoolean.

"category: printing"

- printOn: aStream
    aStream nextPutAll: 'true'.
