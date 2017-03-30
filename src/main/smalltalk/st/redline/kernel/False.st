"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent the logical value false."

Boolean subclass: #False
      	category: 'Kernel-Objects'.

"False methodsFor: 'logical operations'"

- & alternativeObject
    "Evaluating conjunction -- answer false since receiver is false."

    ^ self.

- not
    "Negation -- answer true since the receiver is false."

    ^ true.

- | aBoolean
    "Evaluating disjunction (OR) -- answer with the argument, aBoolean."

    ^ aBoolean.


"False methodsFor: 'controlling'"

- and: alternativeBlock
    "Nonevaluating conjunction -- answer with false since the receiver is false."

    ^ self.

- ifFalse: alternativeBlock
    "Answer the value of alternativeBlock. Execution does not actually
    reach here because the expression is compiled in-line."

    ^ alternativeBlock value.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
    "Answer the value of falseAlternativeBlock. Execution does not
    actually reach here because the expression is compiled in-line."

    ^ falseAlternativeBlock value.

- ifTrue: alternativeBlock
    "Since the condition is false, answer the value of the false alternative,
    which is nil. Execution does not actually reach here because the
    expression is compiled in-line."

    ^ nil.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
    "Answer the value of falseAlternativeBlock. Execution does not
    actually reach here because the expression is compiled in-line."

    ^ falseAlternativeBlock value.

- or: alternativeBlock
	"Nonevaluating disjunction -- answer value of alternativeBlock."

	^ alternativeBlock value.

"False methodsFor: 'printing'"

- printOn: aStream

    aStream nextPutAll: 'false'.
