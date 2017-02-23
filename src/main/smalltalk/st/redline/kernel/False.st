"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Boolean subclass: #False.

- & alternativeObject
	^ self.

- and: alternativeBlock
	^ self.

- ifFalse: alternativeBlock
	^ alternativeBlock value.

- ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock
	^ falseAlternativeBlock value.

- ifTrue: alternativeBlock
	^ nil.

- ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	^ falseAlternativeBlock value.

- not
	^ true.

- or: alternativeBlock
	^ alternativeBlock value.

- printOn: aStream
	aStream nextPutAll: 'false'.

- | aBoolean
	^ aBoolean.
