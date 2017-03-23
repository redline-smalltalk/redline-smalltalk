"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Boolean.

+ new
	self error: 'You may not create any more Booleans - this is two-valued logic'.

- & aBoolean
	self subclassResponsibility.

- and: alternativeBlock
	self subclassResponsibility.

- deepCopy
	^ self.

- eqv: aBoolean
	^ self == aBoolean.

- ifFalse: alternativeBlock
	self subclassResponsibility.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	self subclassResponsibility.

- ifTrue: alternativeBlock
	self subclassResponsibility.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	self subclassResponsibility.

- not
	self subclassResponsibility.

- or: alternativeBlock
	self subclassResponsibility.

- shallowCopy
	^ self.

- storeOn: aStream
	self printOn: aStream.

- | aBoolean
	self subclassResponsibility.
