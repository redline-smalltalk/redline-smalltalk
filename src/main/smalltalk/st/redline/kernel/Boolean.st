"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Boolean.

"category: controlling"

- and: alternativeBlock
    "Nonevaluating conjunction. If the receiver is true, answer the value of the argument, alternativeBlock; otherwise
     answer false without evaluating the argument."
    self subclassResponsibility.

- ifFalse: alternativeBlock
    "If the receiver is true (i.e., the condition is true), then the value is the true alternative, which is nil.
     Otherwise answer the result of evaluating the argument, alternativeBlock. Create an error notification if the
     receiver is nonBoolean. Execution does not actually reach here because the expression is compiled in-line."
    self subclassResponsibility.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
    "Same as ifTrue:ifFalse:."
    self subclassResponsibility.

- ifTrue: alternativeBlock
    "If the receiver is false (i.e., the condition is false), then the value is the false alternative, which is nil.
     Otherwise answer the result of evaluating the argument, alternativeBlock. Create an error notification if the
     receiver is nonBoolean. Execution does not actually reach here because the expression is compiled in-line."
    self subclassResponsibility.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
    "If the receiver is true (i.e., the condition is true), then answer the value of the argument trueAlternativeBlock.
     If the receiver is false, answer the result of evaluating the argument falseAlternativeBlock. If the receiver is
     a nonBoolean then create an error notification."
    self subclassResponsibility.

- or: alternativeBlock
    "Nonevaluating disjunction. If the receiver is false, answer the value of the argument, alternativeBlock; otherwise
     answer true without evaluating the argument."
    self subclassResponsibility.

"category: copying"

- deepCopy
    "Receiver has two concrete subclasses, True and False. Only one instance of each should be made, so return self."

- shallowCopy
    "Receiver has two concrete subclasses, True and False. Only one instance of each should be made, so return self."

"category: storing"

- storeOn: aStream
    "Refer to the comment in Object|storeOn:."
    self printOn: aStream.
