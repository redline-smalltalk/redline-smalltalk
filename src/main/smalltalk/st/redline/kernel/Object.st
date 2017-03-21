"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

PrimObject subclass: #Object.

+ newFrom: aSimilarObject
	| myInstVars similarInstVars any inst good |
	myInstVars := self allInstVarNames.
	similarInstVars := aSimilarObject class allInstVarNames.
	inst := self new.
	myInstVars
		doWithIndex: [ :each :index |
			good := similarInstVars indexOf: each.
			good > 0 ifTrue: [
				  inst instVarAt: index put: (aSimilarObject instVarAt: good).
					any := true
			]].
	any == nil ifTrue: [self subclassResponsibility].
	^ inst.

+ readFrom: aStream
	| object |
	object := Compiler evaluate: aStream.
	(object isKindOf: self)
		ifFalse: [self error: self name + ' expected'].
	^ object.

- = anObject
	^ self == anObject.

- == anObject
	JVM primitive: 110.

- addDependent: anObject
	| dependents |
	dependents := self dependents.
	dependents do: [ :o |
		o == anObject ifTrue: [^ self].
		nil
	].
	DependentsFields at: self put: (dependents copyWith: anObject).

- as: aSimilarClass
	^ aSimilarClass newFrom: self.

- asOrderedCollection
	^ (OrderedCollection new: 1) add: self;
	 yourself.

- asString
	^ self printString.

- at: index
	JVM primitive: 60.

- at: index put: value
	JVM primitive: 61.

- atPin: index
	JVM primitive: 60.

- atWrap: index
	JVM primitive: 60.

- basicAt: index
	JVM primitive: 60.

- basicAt: index put: value
	JVM primitive: 61.

- basicSize
	JVM primitive: 62.

- become: otherObject
	(Array with: self)
		elementsExchangeIdentityWith: (Array with: otherObject).

- bindingOf: aString
  ^ nil.

- breakDependents
  DependentsFields removeKey: self.
  " TODO-JCL: was DependentsFields removeKey: self ifAbsent: []."

- caseError
	self error: 'Case not found, and no otherwise clause'.

- caseOf: aBlockAssociationCollection
	^ self caseOf: aBlockAssociationCollection.

- caseOf: aBlockAssociationCollection otherwise: aBlock
	aBlockAssociationCollection
		associationsDo: [ :assoc |
			assoc key value = self ifTrue: [^ assoc value value].
			nil ].
	^ aBlock value.

- changed
	self changed: self.

- changed: aParameter
	self dependents do: [:aDependent | aDependent update: aParameter].

- class
	JVM primitive: 111.

- copy
	^ self shallowCopy.

- deepCopy
	JVM primitive: 301.

- dependents
	(DependentsFields includesKey: self)
		ifTrue: [^ DependentsFields at: self].
	^ #().

