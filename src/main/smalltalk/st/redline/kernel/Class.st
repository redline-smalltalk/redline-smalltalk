"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ClassDescription subclass: #Class
	instanceVariableNames: 'name classPool sharedPools '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'.

" TODO.JCL -
  addClassVarName: aString
  copy
  copyForValidation
  declare: varString
  fileOutPool: aPool onFileStream: aFileStream
  fileOutSharedPoolsOn: aFileStream.
  possibleVariablesFor: misspelled continuedFrom: oldResults
  removeClassVarName: aString
  removeFromSystem
  rename: aString
  scopeHas: varName ifTrue: assocBlock
  sharing: poolString
  shouldFileOutPool: aPoolName
  shouldFileOutPools
  superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet
  validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods wasPresent: wasPresent.
  variableByteSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat.
  variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat.
  variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat.
"

- classPool
	classPool == nil ifTrue: [^ Dictionary new].
	^ classPool.

- classVarNames
	^ self classPool keys.

- compileAllFrom: oldClass
	super compileAllFrom: oldClass.
	self class compileAllFrom: oldClass class.

- fileOut
	^ self fileOutAsHtml: false.

- fileOutAsHtml: useHtml
	| fileStream |
	fileStream := useHtml
				ifTrue: [(FileStream newFileNamed: self name + FileDirectory dot + 'html') asHtml]
				ifFalse: [FileStream newFileNamed: self name + FileDirectory dot + 'st'].
	fileStream header; timeStamp.
	self sharedPools size > 0
		ifTrue: [self shouldFileOutPools ifTrue: [self fileOutSharedPoolsOn: fileStream]].
	self
		fileOutOn: fileStream
		moveSource: false
		toFile: 0.
	fileStream trailer; close.

- fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
	Transcript cr; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex.
	self class nonTrivial
		ifTrue:
			[aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex].

- hasMethods
	^ super hasMethods or: [self class hasMethods].

- initialize
	^ self.

- name
	JVM primitive: 308.

- obsolete
	name := 'AnObsolete' + name.
	classPool := Dictionary new.
	self class obsolete.
	super obsolete.

- sharedPools
	sharedPools == nil ifTrue: [^ OrderedCollection new].
	^ sharedPools.

- subclass: className
    "Shorthand for subclass creation."
    ^ self subclass: className instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: ''.

- subclass: className instanceVariableNames: ivn classVariableNames: cvn poolDictionaries: pd category: cat
    "Create a new class that is a subclass of the receiver. Each or the arguments provides the information needed to
     initialize the new class and categorize it."
    JVM aload: 1;
        aload: 2;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveSubclass' matching: '(Lst/redline/core/PrimContext;)Lst/redline/core/PrimObject;'.

+ template: category
	^ 'Object subclass: #NameOfClass
	instanceVariableNames: ''instVarName1 instVarName2''
	classVariableNames: ''ClassVarName1 ClassVarName2''
	poolDictionaries: ''''
	category: ''' + category + ''''.
