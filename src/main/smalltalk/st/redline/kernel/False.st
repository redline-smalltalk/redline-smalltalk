"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Boolean subclass: #False.

"category: initialization"

- initialize
    "Mark the resulting objects falseness so internally we can distinguish a False instance."
    JVM aload: 1;
        invokeVirtual: 'st/redline/lang/ProtoObject' method: 'markFalseness' matching: '()Lst/redline/lang/ProtoObject;'.

"category: controlling"

- and: alternativeBlock
    "Nonevaluating conjunction -- answer with false since the receiver is false."
    ^ self.

- ifTrue: alternativeBlock
    "Since the condition is false, the value is the false alternative, which is nil."
    ^ nil.

- ifFalse: alternativeBlock
    "Answer the value of alternativeBlock."
    ^ alternativeBlock value.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
    "Answer the value of falseAlternativeBlock."
    ^ falseAlternativeBlock value.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
    "Answer the value of falseAlternativeBlock."
    ^ falseAlternativeBlock value.

- or: alternativeBlock
    "Nonevaluating disjunction -- answer value of alternativeBlock."
    ^ alternativeBlock value.

"category: logical operations"

- & alternativeObject
    "Evaluating conjunction -- answer false since receiver is false."
    ^ self.

- not
    "Negation -- answer true since the receiver is false."
    ^ true.

- | aBoolean
    "Evaluating disjunction (OR) -- answer with the argument, aBoolean."
    ^ aBoolean.

"category: printing"

- printOn: aStream
    aStream nextPutAll: 'false'.
