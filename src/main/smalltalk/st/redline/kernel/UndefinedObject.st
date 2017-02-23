"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #UndefinedObject.

+ new
	self error: 'You may not create any more undefined objects--use nil'.

- addDependent: ignored
	self error: 'Nil should not have dependents'.

- deepCopy
	^ self.

- ifNil: aBlock
	^ aBlock value.

- ifNil: nilBlock ifNotNil: ifNotNilBlock
	^ nilBlock value.

- ifNotNil: aBlock
	^ self.

- ifNotNil: ifNotNilBlock ifNil: nilBlock
	^ nilBlock value.

- isNil
	^ true.

- notNil
	^ false.

- printOn: aStream
	aStream nextPutAll: 'nil'.

- release
	^ self.

- removeSubclass: aClass
	^ self.

- shallowCopy
	^ self.

- storeOn: aStream
	aStream nextPutAll: 'nil'.

- subclass: nameOfClass instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDictnames category: category
	| newClass |
	newClass := Object
				subclass: nameOfClass
				instanceVariableNames: instVarNames
				classVariableNames: classVarNames
				poolDictionaries: poolDictnames
				category: category.
	Object removeSubclass: newClass.
	newClass superclass: nil.
	^ newClass.
