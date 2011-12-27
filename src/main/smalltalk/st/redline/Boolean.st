" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Boolean
	
- & aBoolean 
  "Evaluating conjunction. Evaluate the argument. Then answer true if 
  both the receiver and the argument are true."

  self subclassResponsibility

- eqv: aBoolean 
  "Answer true if the receiver is equivalent to aBoolean."

  ^self == aBoolean

- not
  "Negation. Answer true if the receiver is false, answer false if the 
  receiver is true."

  self subclassResponsibility

- xor: aBoolean 
  "Exclusive OR. Answer true if the receiver is not equivalent to aBoolean."

  ^(self == aBoolean) not

" TODO The vertical bar (|) operator produces a parse error and NPE: "
" - | aBoolean "
  "Evaluating disjunction (OR). Evaluate the argument. Then answer true 
  if either the receiver or the argument is true."

"  self subclassResponsibility"

- and: alternativeBlock 
  "Nonevaluating conjunction. If the receiver is true, answer the value of 
  the argument, alternativeBlock; otherwise answer false without 
  evaluating the argument."

  self subclassResponsibility

- ifFalse: alternativeBlock 
  "If the receiver is true (i.e., the condition is true), then the value is the 
  true alternative, which is nil. Otherwise answer the result of evaluating 
  the argument, alternativeBlock. Create an error notification if the 
  receiver is nonBoolean. Execution does not actually reach here because 
  the expression is compiled in-line."

  self subclassResponsibility

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
  "Same as ifTrue:ifFalse:."

  self subclassResponsibility

- ifTrue: alternativeBlock 
  "If the receiver is false (i.e., the condition is false), then the value is the 
  false alternative, which is nil. Otherwise answer the result of evaluating 
  the argument, alternativeBlock. Create an error notification if the 
  receiver is nonBoolean. Execution does not actually reach here because 
  the expression is compiled in-line."

  self subclassResponsibility

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
  "If the receiver is true (i.e., the condition is true), then answer the value 
  of the argument trueAlternativeBlock. If the receiver is false, answer the 
  result of evaluating the argument falseAlternativeBlock. If the receiver 
  is a nonBoolean then create an error notification. Execution does not 
  actually reach here because the expression is compiled in-line."

  self subclassResponsibility

- or: alternativeBlock 
  "Nonevaluating disjunction. If the receiver is false, answer the value of 
  the argument, alternativeBlock; otherwise answer true without 
  evaluating the argument."

  self subclassResponsibility 
  
- deepCopy 
  "Receiver has two concrete subclasses, True and False.
  Only one instance of each should be made, so return self."

- shallowCopy 
  "Receiver has two concrete subclasses, True and False.
  Only one instance of each should be made, so return self."

- storeOn: aStream 
  "Refer to the comment in Object|storeOn:."

  self printOn: aStream