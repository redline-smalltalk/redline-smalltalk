"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Behavior
	     instanceVariableNames: 'superclass methodDict format subclasses '
	     classVariableNames: ''
	     poolDictionaries: ''
	     category: 'Kernel-Classes'.

- addSelector: selector withMethod: compiledMethod
	self basicAddSelector: selector withMethod: compiledMethod.

- addSubclass: aSubclass
	aSubclass superclass ~~ self
		ifTrue: [self error: aSubclass name + ' is not my subclass']
		ifFalse: [subclasses == nil
				ifTrue: [subclasses := Set with: aSubclass]
				ifFalse: [subclasses add: aSubclass]].

"TODO.JCL
  allCallsOn: aSymbol
  allInstancesDo: aBlock
  allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level
  becomeCompact
  becomeUncompact
  browseAll**
  changeRecordsAt: selector
  confirmRemovalOf: aSelector
  firstCommentAt: selector
  format: nInstVars variable: isVar words: isWords pointers: isPointers
  indexIfCompact
  inheritsFrom: aClass
  inspect**
  instSize
  instSpec
  isBits
  isBytes
  isPointers
  isVariable
  isWords
  literalScannedAs: scannedLiteral notifying: requestor
"

- allInstVarNames
	| vars |
	superclass == nil
		ifTrue: [vars := self instVarNames copy]
		ifFalse: [vars := superclass allInstVarNames + self instVarNames].
	^ vars.

- allInstances
	| all |
	all := OrderedCollection new.
	self
		allInstancesDo: [:x | x == all ifFalse: [all add: x]].
	^ all asArray.

- allSelectors
	| temp |
	superclass == nil ifTrue: [^ self selectors].
	temp := superclass allSelectors.
	temp addAll: self selectors.
	^ temp.

- allSubInstances
	| aCollection |
	aCollection := OrderedCollection new.
	self
		allSubInstancesDo: [:x | x == aCollection ifFalse: [aCollection add: x]].
	^ aCollection.

- allSubInstancesDo: aBlock
	self allInstancesDo: aBlock.
	self allSubclassesDo: [:sub | sub allInstancesDo: aBlock].

- allSubclasses
  	| aSet |
  	aSet := Set new.
  	aSet addAll: self subclasses.
  	self subclasses do: [:eachSubclass | aSet addAll: eachSubclass allSubclasses].
  	^ aSet.

- allSubclassesDo: aBlock
	self
		subclassesDo:
			[:cl |
			aBlock value: cl.
			cl allSubclassesDo: aBlock].

- allSuperclasses
	| temp |
	superclass == nil ifTrue: [^ OrderedCollection new].
	temp := superclass allSuperclasses.
	temp addFirst: superclass.
	^ temp.

- new
  "Answer a new initialized instance of the receiver."
  ^ self basicNew initialize.

- new: anInteger
	^ (self basicNew: anInteger) initialize.

- basicNew
    JVM aload: 1;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveNew' matching: '()Lst/redline/core/PrimObject;'.

- basicNew: anInteger
    JVM aload: 1;
        argLoad: 0;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveNew' matching: '(Lst/redline/core/PrimObject;)Lst/redline/core/PrimObject;'.

- canUnderstand: selector
	(self includesSelector: selector)
		ifTrue: [^ true].
	superclass == nil ifTrue: [^ false].
	^ superclass canUnderstand: selector.

- classVarNames
	^ Set new.

- compileAllFrom: oldClass
	self selectorsDo: [:sel | self recompile: sel from: oldClass].

- compiledMethodAt: selector
	^ methodDict at: selector.

- compiledMethodAt: selector ifAbsent: aBlock
	^ methodDict at: selector ifAbsent: [aBlock value].

- compilerClass
	^ Compiler.

- copy
	| myCopy savedSubclasses |
	savedSubclasses := subclasses.
	subclasses := nil.
	myCopy := self shallowCopy.
	subclasses := savedSubclasses.
	^ myCopy methodDictionary: methodDict copy.

- decompilerClass
	^ Decompiler.

- defaultSelectorForMethod: aMethod
	| aStream |
	aStream := WriteStream on: (String new: 16).
	aStream nextPutAll: 'DoIt'.
	1 to: aMethod numArgs do: [:i | aStream nextPutAll: 'with:'].
	^ aStream contents asSymbol.

- evaluatorClass
	^ Compiler.

- flushCache
	JVM primitive: 89.

- format
	^ format.

- hasMethods
	^ methodDict size > 0.

- includesSelector: aSymbol
	^ methodDict includesKey: aSymbol.

- instVarNames
	| mySize superSize |
	mySize := self instSize.
	superSize := superclass == nil
				ifTrue: [0]
				ifFalse: [superclass instSize].
	mySize = superSize ifTrue: [^ #()].
	^ (superSize + 1 to: mySize)
		collect: [:i | 'inst' + i printString].

- instanceCount
	| count |
	count := 0.
	self allInstancesDo: [:x | count := count + 1].
	^ count.

- kindOfSubclass
	self isVariable
		ifTrue:
			[self isBits
				ifTrue:
					[self isBytes ifTrue: [^ ' variableByteSubclass: '].
					^ ' variableWordSubclass: '].
			^ ' variableSubclass: '].
	^ ' subclass: '.

- methodDictionary: aDictionary
	methodDict := aDictionary.

- obsolete
	methodDict := MethodDictionary new.
	superclass == nil ifFalse: [superclass removeSubclass: self].

- parserClass
	^ self compilerClass parserClass.

- printHierarchy
	| aStream index |
	index := 0.
	aStream := WriteStream on: (String new: 16).
	self allSuperclasses
		reverseDo:
			[:aClass |
			aStream crtab: index.
			index := index + 1.
			aStream nextPutAll: aClass name.
			aStream space.
			aStream print: aClass instVarNames].
	aStream cr.
	self printSubclassesOn: aStream level: index.
	^ aStream contents.

- printOn: aStream
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream.

- printSubclassesOn: aStream level: level
	| subclassNames subclass |
	aStream crtab: level.
	aStream nextPutAll: self name.
	aStream space; print: self instVarNames.
	self == Class
		ifTrue:
			[aStream crtab: level + 1;
			 nextPutAll: '[ ... all the Metaclasses ... ]'.
			^ self].
	subclassNames := self subclasses collect: [:subC | subC name].
	subclassNames asSortedCollection do:
		[:name |
		subclass := self subclasses detect: [:subC | subC name = name].
		subclass printSubclassesOn: aStream level: level + 1].

- recompile: selector from: oldClass
	| method trailer methodNode |
	method := self compiledMethodAt: selector.
	trailer := (method size - 3 to: method size)
				collect: [:i | method at: i].
	methodNode := self compilerClass new
				compile: (oldClass sourceCodeAt: selector)
				in: self
				notifying: nil
				ifFail: [^ self].
	selector == methodNode selector ifFalse: [self error: 'selector changed!'].
	self addSelector: selector withMethod: (methodNode generate: trailer).

- removeSelector: selector
	methodDict removeKey: selector.
	selector flushCache.

- removeSelectorSimply: selector
	methodDict removeKey: selector ifAbsent: [^ self].
	selector flushCache.

- removeSubclass: aSubclass
	subclasses == nil
		ifFalse:
			[subclasses remove: aSubclass.
			subclasses isEmpty ifTrue: [subclasses := nil]].
	Object flushCache.

- scopeHas: name ifTrue: assocBlock
	^ superclass scopeHas: name ifTrue: assocBlock.

- selectorAtMethod: method setClass: classResultBlock
	| sel |
	sel := methodDict keyAtValue: method
				ifAbsent:
					[superclass == nil
						ifTrue:
							[classResultBlock value: self.
							^ self defaultSelectorForMethod: method].
					sel := superclass selectorAtMethod: method setClass: classResultBlock.
					sel == (self defaultSelectorForMethod: method) ifTrue: [classResultBlock value: self].
					^ sel].
	classResultBlock value: self.
	^ sel.

- selectors
	^ methodDict keys.

- selectorsDo: selectorBlock
	^ methodDict keysDo: selectorBlock.

- sharedPools
	^ OrderedCollection new.

- someInstance
	JVM primitive: 77.

- sourceCodeAt: selector
	^ (methodDict at: selector)
		getSourceFor: selector in: self.

- sourceCodeAt: selector ifAbsent: aBlock
	^ (methodDict at: selector ifAbsent: [^ aBlock value])
		getSourceFor: selector in: self.

- sourceCodeTemplate
	^ 'message selector and argument names
	"comment stating purpose of message"
	| temporary variable names |
	statements.'.

- sourceMethodAt: selector ifAbsent: aBlock
	^ (self sourceCodeAt: selector ifAbsent: [^ aBlock value]) asText makeSelectorBoldIn: self.

- subclassDefinerClass
	^ Compiler.

- subclasses
	subclasses == nil ifTrue: [^ Set new].
	^ subclasses copy.

- subclassesDo: aBlock
	subclasses == nil ifFalse: [subclasses do: [:cl | aBlock value: cl]].

