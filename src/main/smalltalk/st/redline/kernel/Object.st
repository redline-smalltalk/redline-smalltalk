"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

PrimObject subclass: #Object
           instanceVariableNames: ''
           classVariableNames: 'DependentsFields '
           poolDictionaries: ''
           category: 'Kernel-Objects'.

- initialize
  ^ self.

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

- do: aBlock
	self notify: 'Only collections should respond to "do:". The "do:" method in Object will soon be removed; clients depending on it should use "in:" instead. If you get this message in system code, please report it to johnm@wdi.disney.com. Proceed from this notifier to get the old behavior.'.
	^ self in: aBlock.

- error: aString
  JVM primitive: 302.

- errorImproperStore
	self error: 'Improper store into indexable object'.

- errorNonIntegerIndex
	self error: 'only integers should be used as indices'.

- errorSubscriptBounds: index
	self error: 'subscript is out of bounds: ' + index printString.

- flag: aSymbol
	^ self.

- halt
	JVM primitive: 303.

- halt: aString
	JVM primitive: 303.

- hash
	^ self identityHash.

- identityHash
	JVM primitive: 75.

- ifNil: nilBlock
	^ self.

- ifNil: nilBlock ifNotNil: ifNotNilBlock
	^ ifNotNilBlock value.

- ifNotNil: ifNotNilBlock
	^ ifNotNilBlock value.

- ifNotNil: ifNotNilBlock ifNil: nilBlock
	^ ifNotNilBlock value.

- in: aBlock
	^ aBlock value: self.

- inline: inlineFlag
	^ self.

- instVarAt: index
	JVM primitive: 73.

- instVarAt: anInteger put: anObject
	JVM primitive: 74.

- isInteger
	^ false.

- isKindOf: aClass
	self class == aClass ifTrue: [^ true].
	^ self class inheritsFrom: aClass.

- isLiteral
	^ false.

- isMemberOf: aClass
	^ self class == aClass.

- isNil
	^ false.

- isNumber
	^ false.

- isPseudoContext
	^ false.

- isText
	^ false.

- longPrintOn: aStream
	self class allInstVarNames
		doWithIndex:
			[ :title :index |
			aStream nextPutAll: title;
			 nextPut: $:;
			 space;
			 tab.
			(self instVarAt: index)
				printOn: aStream.
			aStream cr].

- longPrintString
	| aStream |
	aStream := WriteStream on: (String new: 100).
	self longPrintOn: aStream.
	^ aStream contents.

- modelWakeUp
	^ self.

- name
	^ self printString.

- nextInstance
	JVM primitive: 78.

- nextObject
	JVM primitive: 139.

- notNil
	^ true.

- notYetImplemented
	self inform: 'Not yet implemented'.

- notify: aString
  JVM primitive: 304.

- notify: aString at: location
	self notify: aString.

- okToChange
	^ true.

- perform: aSymbol
	JVM primitive: 83.

- perform: aSymbol with: anObject
	JVM primitive: 83.

- perform: aSymbol with: firstObject with: secondObject
	JVM primitive: 83.

- perform: aSymbol with: firstObject with: secondObject with: thirdObject
	JVM primitive: 83.

- perform: selector withArguments: anArray
	JVM primitive: 84.

- pointsTo: anObject
	JVM primitive: 132.

- primitiveError: aString
  JVM primitive: 305.

- primitiveFailed
	self error: 'a primitive has failed'.

- printOn: aStream
	| title |
	title := self class name.
	aStream nextPutAll: ((title at: 1) isVowel
			ifTrue: ['an ']
			ifFalse: ['a '])
			+ title.

- printString
	| limit limitedString |
	limit := 50000.
	limitedString := String streamContents: [:s | self printOn: s]
				limitedTo: limit.
	limitedString size < limit ifTrue: [^ limitedString].
	^ limitedString + '...etc...'.

- readFromString: aString
	^ self readFrom: (ReadStream on: aString).

- rehash
	^ self.

- release
	self breakDependents.

- removeDependent: anObject
  self error: 'implement me'.
  " TODO-JCL: Implement."

- respondsTo: aSymbol
  ^ self class canUnderstand: aSymbol.

- shallowCopy
	JVM primitive: 148.

- shouldNotImplement
	self error: 'This message is not appropriate for this object'.

- size
	JVM primitive: 62.

- someObject
	JVM primitive: 138.

- species
	^ self class.

- storeAt: offset inTempFrame: aContext
	^ aContext tempAt: offset put: self.

- storeOn: aStream
	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(' , self class name , ' basicNew: ';
			 store: self basicSize;
			 nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name , ' basicNew'].
	1 to: self class instSize do: [:i | aStream nextPutAll: ' instVarAt: ';
		 store: i;
		 nextPutAll: ' put: ';
		 store: (self instVarAt: i);
		 nextPut: $;].
	1 to: self basicSize do: [:i | aStream nextPutAll: ' basicAt: ';
		 store: i;
		 nextPutAll: ' put: ';
		 store: (self basicAt: i);
		 nextPut: $;].
	aStream nextPutAll: ' yourself)'.

- storeString
	^ String streamContents: [:s | self storeOn: s].

- subclassResponsibility
	self error: 'My subclass should have overridden one of my messages.'.

- tryPrimitive: primIndex withArgs: argumentArray
	JVM primitive: 118.

- update: aParameter
	^ self.

- var: varSymbol declareC: declString
	^ self.

- yourself
	^ self.

- ~= anObject
	^ self = anObject == false.

- ~~ anObject
	self == anObject ifTrue: [^ false].
	^ true.

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
