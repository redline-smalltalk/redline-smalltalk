"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Boolean subclass: #True.

- & alternativeObject
	^ alternativeObject.

- and: alternativeBlock
	^ alternativeBlock value.

- ifFalse: alternativeBlock
	^ nil.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	^ trueAlternativeBlock value.

- ifTrue: alternativeBlock
	^ alternativeBlock value.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	^ trueAlternativeBlock value.

- not
	^ false.

- or: alternativeBlock
	^ self.

- printOn: aStream
	aStream nextPutAll: 'true'.

- | aBoolean
	^ self.
