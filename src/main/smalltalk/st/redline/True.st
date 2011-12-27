" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Boolean < #True

- & alternativeObject 
  "Evaluating conjunction -- answer alternativeObject since receiver is true."

  ^alternativeObject

- not
  "Negation--answer false since the receiver is true."

  ^false

" TODO The vertical bar (|) operator produces a parse error and NPE: "
" - | aBoolean "
  "Evaluating disjunction (OR) -- answer true since the receiver is true."

"  ^self "

- and: alternativeBlock 
  "Nonevaluating conjunction -- answer the value of alternativeBlock since
  the receiver is true."

  ^alternativeBlock value

- ifFalse: alternativeBlock 
  "Since the condition is true, the value is the true alternative, which is nil. 
  Execution does not actually reach here because the expression is compiled 
  in-line."

  ^nil

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
  "Answer the value of trueAlternativeBlock. Execution does not 
  actually reach here because the expression is compiled in-line."

  ^trueAlternativeBlock value

- ifTrue: alternativeBlock 
  "Answer the value of alternativeBlock. Execution does not actually 
  reach here because the expression is compiled in-line."

  ^alternativeBlock value

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
  "Answer with the value of trueAlternativeBlock. Execution does not 
  actually reach here because the expression is compiled in-line."

  ^trueAlternativeBlock value

- or: alternativeBlock 
  "Nonevaluating disjunction -- answer true since the receiver is true."

  ^self

- printOn: aStream 

  aStream nextPutAll: 'true'