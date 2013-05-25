"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Boolean subclass: #True.

"category: initializing"

- initialize
    "Mark the resulting objects trueness so internally we can distinguish a True instance."
    JVM aload: 1;
        invokeVirtual: 'st/redline/lang/ProtoObject' method: 'markTrueness' matching: '()Lst/redline/lang/ProtoObject;'.

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

"category: printing"

- printOn: aStream
    aStream nextPutAll: 'true'.
