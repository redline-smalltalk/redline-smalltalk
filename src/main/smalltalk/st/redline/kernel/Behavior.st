"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and
 creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but
 I am a good starting point for providing instance-specific behavior (as in Metaclass)."

Object subclass: #Behavior
       instanceVariableNames: 'superclass methodDict format subclasses '
       classVariableNames: ''
       poolDictionaries: ''
       category: 'Kernel-Classes'.

"Behavior methodsFor: 'initialize-release'"

- obsolete
    "Invalidate and recycle local messages. Remove the receiver from its
    superclass' subclass list."

    methodDict := MethodDictionary new.
    superclass == nil ifFalse: [superclass removeSubclass: self].


"Behavior methodsFor: 'accessing'"

- compilerClass
    "Answer a compiler class appropriate for source methods of this class."

    ^ Compiler.

- confirmRemovalOf: aSelector
    "Determine if it is okay to remove the given selector.  Answer 1 if it should be removed, 2 if it should be removed
     followed by a senders browse, and 3 if it should not be removed."

    | count aMenu answer caption allCalls |
    (count := (allCalls := Smalltalk allCallsOn: aSelector) size) > 0
      ifTrue:
        [aMenu := PopUpMenu labels: 'Remove it
  Remove, then browse senders
  Don''t remove, but show me those senders
  Forget it -- do nothing -- sorry I asked'.

        caption := 'This message has ' + count printString + ' sender'.
        count > 1 ifTrue:
          [caption := caption copyWith: $s].
        answer := aMenu startUpWithCaption: caption.
        answer == 3 ifTrue:
          [Smalltalk browseMessageList: allCalls
            name: 'Senders of ' + aSelector
            autoSelect: aSelector keywords first].
        answer == 0 ifTrue: [answer := 3].  "If user didn't answer, treat it as cancel"
        ^ answer min: 3]
      ifFalse:
        [^ 1].

- decompilerClass
    "Answer a decompiler class appropriate for compiled methods of this class."

    ^ Decompiler.

- evaluatorClass
    "Answer an evaluator class appropriate for evaluating expressions in the
    context of this class."

    ^ Compiler.

- format
	"Answer an Integer that encodes the kinds and numbers of variables of
	instances of the receiver."

	^ format.

- parserClass
    "Answer a parser class to use for parsing method headers."

    ^ self compilerClass parserClass.

- sourceCodeTemplate
    "Answer an expression to be edited and evaluated in order to define
    methods in this class."

    ^ 'message selector and argument names
    "comment stating purpose of message"

    | temporary variable names |
    statements.'.

- subclassDefinerClass
    "Answer an evaluator class appropriate for evaluating definitions of new
    subclasses of this class."

    ^ Compiler.


"Behavior methodsFor: 'testing'"

- instSize
    "Answer the number of named instance variables
    (as opposed to indexed variables) of the receiver."

    JVM primitive: 444.

- instSpec
	JVM primitive: 445.

- isBits
    "Answer whether the receiver contains just bits (not pointers)."

    ^ self instSpec >= 6.

- isBytes
    "Answer whether the receiver has 8-bit instance variables."

    ^ self instSpec >= 8.

- isFixed
    "Answer whether the receiver does not have a variable (indexable) part."

    ^ self isVariable not.

- isPointers
    "Answer whether the receiver contains just pointers (not bits)."

    ^ self isBits not.

- isVariable
    "Answer whether the receiver has indexable variables."

    ^ self instSpec >= 2.

- isWords
    "Answer whether the receiver has 16-bit instance variables."

    ^ self isBytes not.


"Behavior methodsFor: 'copying'"

- copy
    "Answer a copy of the receiver without a list of subclasses."

    | myCopy savedSubclasses |
    savedSubclasses := subclasses.
    subclasses := nil.
    myCopy := self shallowCopy.
    subclasses := savedSubclasses.
    ^ myCopy methodDictionary: methodDict copy.

"Behavior methodsFor: 'printing'"

- defaultNameStemForInstances
    "Answer a basis for names of default instances of the receiver"

    ^ self name.

- literalScannedAs: scannedLiteral notifying: requestor
    "Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
    If scannedLiteral is not an association, answer it.
    Else, if it is of the form:
      nil->#NameOfMetaclass
    answer nil->theMetaclass, if any has that name, else report an error.
    Else, if it is of the form:
      #NameOfGlobalVariable->anythiEng
    answer the global, class, or pool association with that nameE, if any, else
    add it to Undeclared a answer the new Association."

    | key value |
    (scannedLiteral isMemberOf: Association)
      ifFalse: [^ scannedLiteral].
    key := scannedLiteral key.
    value := scannedLiteral value.
    key isNil
      ifTrue: "###<metaclass soleInstance name>"
        [self scopeHas: value ifTrue:
          [:assoc |
           (assoc value isKindOf: Behavior)
            ifTrue: [^ nil->assoc value class]].
         requestor notify: 'No such metaclass'.
         ^false].
    (key isMemberOf: Symbol)
      ifTrue: "##<global var name>"
        [(self scopeHas: key ifTrue: [:assoc | ^assoc])
          ifFalse:
            [Undeclared at: key put: nil.
             ^ Undeclared associationAt: key]].
    requestor notify: '## must be followed by a non-local variable name'.
    ^ false.

- printHierarchy
    "Answer a description containing the names and instance variable names
    of all of the subclasses and superclasses of the receiver."

    | aStream index |
    index := 0.
    aStream := WriteStream on: (String new: 16).
    self allSuperclasses reverseDo:
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
    "Refer to the comment in Object|printOn:."

    aStream nextPutAll: 'a descendent of '.
    superclass printOn: aStream.

- storeLiteral: aCodeLiteral on: aStream
    "Store aCodeLiteral on aStream, changing an Association to ##GlobalName
     or ###MetaclassSoleInstanceName format if appropriate"

    | key value |
    (aCodeLiteral isMemberOf: Association)
      ifFalse:
        [aCodeLiteral storeOn: aStream.
         ^ self].
    key := aCodeLiteral key.
    (key isNil and: [(value := aCodeLiteral value) isMemberOf: Metaclass])
      ifTrue:
        [aStream nextPutAll: '###'; nextPutAll: value soleInstance name.
         ^ self].
    ((key isMemberOf: Symbol) and: [self scopeHas: key ifTrue: [:ignore]])
      ifTrue:
        [aStream nextPutAll: '##'; nextPutAll: key.
         ^ self].
    aCodeLiteral storeOn: aStream.

"Behavior methodsFor: 'creating class hierarchy'"

- addSubclass: aSubclass
    "Make the argument, aSubclass, be one of the subclasses of the receiver.
    Create an error notification if the argument's superclass is not the
    receiver."

    aSubclass superclass ~~ self
      ifTrue: [self error: aSubclass name + ' is not my subclass']
      ifFalse: [subclasses == nil
            ifTrue:	[subclasses := Set with: aSubclass]
            ifFalse: [subclasses add: aSubclass]].


- removeSubclass: aSubclass
    "If the argument, aSubclass, is one of the receiver's subclasses, remove it."

    subclasses == nil ifFalse:
      [subclasses remove: aSubclass ifAbsent: [].
      subclasses isEmpty ifTrue: [subclasses := nil]].
    Object flushCache.

- superclass: aClass
    "Change the receiver's superclass to be aClass."

    JVM primitive: 308.

"Behavior methodsFor: 'creating method dictionary'"

- addSelector: selector withMethod: compiledMethod
    "Add the message selector with the corresponding compiled method to the
    receiver's method dictionary."

    self basicAddSelector: selector withMethod: compiledMethod.

- compile: code
    "Compile the argument, code, as source code in the context of the
    receiver. Create an error notification if the code can not be compiled.
    The argument is either a string or an object that converts to a string or a
    PositionableStream on an object that converts to a string."

    ^ self compile: code notifying: nil.

- compile: code notifying: requestor
    "Compile the argument, code, as source code in the context of the
    receiver and insEtall the result in the receiver's method dictionary. The
    second argument, requestor, is to be notified if an error occurs. The
    argument code is either a string or an object that converts to a string or
    a PositionableStream. This method also saves the source code."

    | method selector methodNode |
    method := self
      compile: code "a Text"
      notifying: requestor
      trailer: #(0 0 0 0)
      ifFail: [^ nil]
      elseSetSelectorAndNode:
        [:sel :parseNode |
         selector := sel.
         methodNode := parseNode].
    method putSource: code "a Text" fromParseNode: methodNode inFile: 2
        withPreamble: [:f | f cr; nextPut: $!!; nextChunkPut: 'Behavior method'; cr].
    ^ selector.

- compileAll

  ^ self compileAllFrom: self.

- compileAllFrom: oldClass
    "Compile all the methods in the receiver's method dictionary.
    This validates sourceCode and variable references and forces
    all methods to use the current bytecode set"

    self selectorsDo: [:sel | self recompile: sel from: oldClass].

- compress
    "Compact the method dictionary of the receiver."

    methodDict rehash.

- decompile: selector
    "Find the compiled code associated with the argument, selector, as a
    message selector in the receiver's method dictionary and decompile it.
    Answer the resulting source code as a string. Create an error notification
    if the selector is not in the receiver's method dictionary."

    ^ self decompilerClass new decompile: selector in: self.

- defaultSelectorForMethod: aMethod
    "Given a method, invent and answer an appropriate message selector (a
    Symbol), that is, one that will parse with the correct number of
    arguments."

    | aStream |
    aStream := WriteStream on: (String new: 16).
    aStream nextPutAll: 'DoIt'.
    1 to: aMethod numArgs do: [:i | aStream nextPutAll: 'with:'].
    ^ aStream contents asSymbol.

- methodDictionary: aDictionary
    "Store the argument, aDictionary, as the method dictionary of the
    receiver."

    methodDict := aDictionary.

- recompile: selector from: oldClass
    "Compile the method associated with selector in the receiver's method dictionary."
    | method trailer methodNode |
    method := self compiledMethodAt: selector.
    trailer := (method size - 3 to: method size) collect: [:i | method at: i].
    methodNode := self compilerClass new
          compile: (oldClass sourceCodeAt: selector)
          in: self
          notifying: nil
          ifFail: [^ self].   "Assume OK after proceed from SyntaxError"
    selector == methodNode selector ifFalse: [self error: 'selector changed!!'].
    self addSelector: selector withMethod: (methodNode generate: trailer).

- recompileChanges
    "Compile all the methods that are in the changes file.
    This validates sourceCode and variable references and forces
    methods to use the current bytecode set"

    self selectorsDo:
      [:sel | (self compiledMethodAt: sel) fileIndex > 1 ifTrue:
        [self recompile: sel from: self]].

- removeSelector: selector
    "Assuming that the argument, selector (a Symbol), is a message selector
    in the receiver's method dictionary, remove it. If the selector is not in
    the method dictionary, create an error notification."

    methodDict removeKey: selector.
    selector flushCache.

"Behavior methodsFor: 'instance creation'"

- basicNew
    "Primitive. Answer an instance of the receiver (which is a class) with no
    indexable variables. Fail if the class is indexable. Essential. See Object
    documentation whatIsAPrimitive."

    JVM aload: 1;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveNew' matching: '()Lst/redline/core/PrimObject;'.

- basicNew: anInteger
    "Primitive. Answer an instance of the receiver (which is a class) with the
    number of indexable variables specified by the argument, anInteger. Fail
    if the class is not indexable or if the argument is not a positive Integer.
    Essential. See Object documentation whatIsAPrimitive."

    JVM aload: 1;
        argLoad: 0;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveNew' matching: '(Lst/redline/core/PrimObject;)Lst/redline/core/PrimObject;'.

- new
	"Answer a new instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."
	"Essential Primitive. See Object documentation whatIsAPrimitive."

  ^ self basicNew initialize.

- new: anInteger
    "Primitive. Answer an instance of the receiver (which is a class) with the
    number of indexable variables specified by the argument, anInteger. Fail
    if the class is not indexable or if the argument is not a positive Integer.
    Essential. See Object documentation whatIsAPrimitive."

    ^ (self basicNew: anInteger) initialize.

"Behavior methodsFor: 'accessing class hierarchy'"

- allSubclasses
    "Answer a Set of the receiver's and the receiver's descendent's subclasses."

    | aSet |
    aSet := Set new.
    aSet addAll: self subclasses.
    self subclasses do: [:eachSubclass | aSet addAll: eachSubclass allSubclasses].
    ^ aSet.
  
!Behavior methodsFor: 'accessing class hierarchy'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level
	"Walk the tree of subclasses, giving the class and its level"
	| subclassNames subclass |
	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order"
	subclassNames _ SortedCollection new.
	self subclassesDo: [:subC | subclassNames add: subC name].
	subclassNames do:
		[:name | (Smalltalk at: name)
			allSubclassesWithLevelDo: classAndLevelBlock
			startingLevel: level+1]! !

!Behavior methodsFor: 'accessing class hierarchy'!
allSuperclasses
	"Answer an OrderedCollection of the receiver's and the receiver's
	ancestor's superclasses. The first element is the receiver's immediate
	superclass, followed by its superclass; the last element is Object."

	| temp |
	superclass == nil
		ifTrue: [^OrderedCollection new]
		ifFalse: [temp _ superclass allSuperclasses.
				temp addFirst: superclass.
				^temp]! !

!Behavior methodsFor: 'accessing class hierarchy'!
subclasses
	"Answer a Set containing the receiver's subclasses."

	subclasses == nil
		ifTrue: [^Set new]
		ifFalse: [^subclasses copy]! !

!Behavior methodsFor: 'accessing class hierarchy'!
superclass
	"Answer the receiver's superclass, a Class."

	^superclass! !

!Behavior methodsFor: 'accessing class hierarchy'!
withAllSubclasses
	"Answer a Set of the receiver, the receiver's descendent's, and the
	receiver's descendent's subclasses."

	| aSet |
	aSet _ Set with: self.
	aSet addAll: self subclasses.
	self subclasses do: [:eachSubclass | aSet addAll: eachSubclass allSubclasses].
	^aSet! !

!Behavior methodsFor: 'accessing class hierarchy'!
withAllSuperclasses
	"Answer an OrderedCollection of the receiver and the receiver's
	superclasses. The first element is the receiver,
	followed by its superclass; the last element is Object."

	| temp |
	temp _ self allSuperclasses.
	temp addFirst: self.
	^ temp! !


!Behavior methodsFor: 'accessing method dictionary'!
allSelectors
	"Answer a Set of all the message selectors that instances of the receiver
	can understand."

	| temp |
	superclass == nil
		ifTrue: [^self selectors]
		ifFalse: [temp _ superclass allSelectors.
				temp addAll: self selectors.
				^temp]

	"Point allSelectors"! !

!Behavior methodsFor: 'accessing method dictionary'!
changeRecordsAt: selector
	"Return a list of ChangeRecords for all versions of the method at selector.
	Source code can be retrieved by sending string to any one"
	"(Pen changeRecordsAt: #go:) collect: [:cRec | cRec string]"
	^ (ChangeList new
			scanVersionsOf: (self compiledMethodAt: selector)
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector)
		changeList! !

!Behavior methodsFor: 'accessing method dictionary'!
compiledMethodAt: selector
	"Answer the compiled method associated with the argument, selector (a
	Symbol), a message selector in the receiver's method dictionary. If the
	selector is not in the dictionary, create an error notification."

	^methodDict at: selector! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 1/15/98 19:34'!
compiledMethodAt: selector ifAbsent: aBlock
	"Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's method dictionary. If the selector is not in the dictionary, return the value of aBlock"

	^ methodDict at: selector ifAbsent: [aBlock value]! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'tk 1/7/98 10:31'!
compressedSourceCodeAt: selector
	"(Paragraph compressedSourceCodeAt: #displayLines:affectedRectangle:) size 721 1921
	Paragraph selectors inject: 0 into: [:tot :sel | tot + (Paragraph compressedSourceCodeAt: sel) size] 13606 31450"
	| rawText parse |
	rawText _ (self sourceCodeAt: selector) asString.
	parse _ self compilerClass new parse: rawText in: self notifying: nil.
	^ rawText compressWithTable:
		((selector keywords ,
		parse tempNames ,
		self instVarNames ,
		#(self super ifTrue: ifFalse:) ,
		((0 to: 7) collect:
			[:i | String streamContents:
				[:s | s cr. i timesRepeat: [s tab]]]) ,
		(self compiledMethodAt: selector) literalStrings)
			asSortedCollection: [:a :b | a size > b size])! !

!Behavior methodsFor: 'accessing method dictionary'!
firstCommentAt:  selector
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but ""clever"" enough to map doubled quotes into a single quote.  5/1/96 sw"
	"Behavior firstCommentAt: #firstCommentAt:"

	| sourceString commentStart  pos nextQuotePos |

	sourceString _ self sourceCodeAt: selector.
	sourceString size == 0 ifTrue: [^ ''].
	commentStart _ sourceString findString: '"' startingAt: 1.
	commentStart == 0 ifTrue: [^ ''].
	pos _ commentStart + 1.
	[(nextQuotePos _ sourceString findString: '"' startingAt: pos) == (sourceString findString: '""' startingAt: pos)]
		whileTrue:
			[pos _ nextQuotePos + 2].

	^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'! !

!Behavior methodsFor: 'accessing method dictionary'!
selectorAtMethod: method setClass: classResultBlock
	"Answer both the message selector associated with the compiled method
	and the class in which that selector is defined."

	| sel |
	sel _ methodDict keyAtValue: method
				ifAbsent:
					[superclass == nil
						ifTrue:
							[classResultBlock value: self.
							^self defaultSelectorForMethod: method].
					sel _ superclass selectorAtMethod: method setClass: classResultBlock.
					"Set class to be self, rather than that returned from
					superclass. "
					sel == (self defaultSelectorForMethod: method) ifTrue: [classResultBlock value: self].
					^sel].
	classResultBlock value: self.
	^sel! !

!Behavior methodsFor: 'accessing method dictionary'!
selectors
	"Answer a Set of all the message selectors specified in the receiver's
	method dictionary."

	^methodDict keys

	"Point selectors."! !

!Behavior methodsFor: 'accessing method dictionary'!
selectorsDo: selectorBlock
	"Evaluate selectorBlock for all the message selectors in my method dictionary."

	^methodDict keysDo: selectorBlock! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'di 1/13/98 17:34'!
sourceCodeAt: selector

	^ (methodDict at: selector) getSourceFor: selector in: self! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 11/3/97 00:09'!
sourceCodeAt: selector ifAbsent: aBlock

	^ (methodDict at: selector ifAbsent: [^ aBlock value]) getSourceFor: selector in: self! !

!Behavior methodsFor: 'accessing method dictionary'!
sourceMethodAt: selector
	"Answer the paragraph corresponding to the source code for the
	argument."

	^(self sourceCodeAt: selector) asText makeSelectorBoldIn: self! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'sw 11/3/97 00:10'!
sourceMethodAt: selector ifAbsent: aBlock
	"Answer the paragraph corresponding to the source code for the
	argument."

	^ (self sourceCodeAt: selector ifAbsent: [^ aBlock value]) asText makeSelectorBoldIn: self! !


!Behavior methodsFor: 'accessing instances and variables'!
allClassVarNames
	"Answer a Set of the names of the receiver's and the receiver's ancestor's
	class variables."

	^superclass allClassVarNames! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jm 5/20/1998 15:53'!
allInstances
	"Answer a collection of all current instances of the receiver."

	| all |
	all _ OrderedCollection new.
	self allInstancesDo: [:x | x == all ifFalse: [all add: x]].
	^ all asArray
! !

!Behavior methodsFor: 'accessing instances and variables'!
allInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The
	Array ordering is the order in which the variables are stored and
	accessed by the interpreter."

	| vars |
	superclass == nil
		ifTrue: [vars _ self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [vars _ superclass allInstVarNames , self instVarNames].
	^vars! !

!Behavior methodsFor: 'accessing instances and variables'!
allSharedPools
	"Answer a Set of the names of the pools (Dictionaries) that the receiver
	and the receiver's ancestors share."

	^superclass allSharedPools! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'di 6/20/97 10:51'!
allSubInstances
	"Answer a list of all current instances of the receiver and all of its subclasses."
	| aCollection |
	aCollection _ OrderedCollection new.
	self allSubInstancesDo:
		[:x | x == aCollection ifFalse: [aCollection add: x]].
	^ aCollection! !

!Behavior methodsFor: 'accessing instances and variables'!
classVarNames
	"Answer a Set of the receiver's class variable names."

	^Set new! !

!Behavior methodsFor: 'accessing instances and variables'!
inspectAllInstances
	"Inpsect all instances of the receiver.  1/26/96 sw"

	| all allSize prefix |
	all _ self allInstances.
	(allSize _ all size) == 0 ifTrue: [^ self notify: 'There are no
instances of ', self name].
	prefix _ allSize == 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].

	all asArray inspectWithLabel: (prefix, ' of ', self name)! !

!Behavior methodsFor: 'accessing instances and variables'!
inspectSubInstances
	"Inspect all instances of the receiver and all its subclasses.  CAUTION - don't do this for something as generic as Object!!  1/26/96 sw"

	| all allSize prefix |
	all _ self allSubInstances.
	(allSize _ all size) == 0 ifTrue: [^ self notify: 'There are no
instances of ', self name, '
or any of its subclasses'].
	prefix _ allSize == 1
		ifTrue: 	['The lone instance']
		ifFalse:	['The ', allSize printString, ' instances'].

	all asArray inspectWithLabel: (prefix, ' of ', self name, ' & its subclasses')! !

!Behavior methodsFor: 'accessing instances and variables'!
instanceCount
	"Answer the number of instances of the receiver that are currently in
	use."

	| count |
	count _ 0.
	self allInstancesDo: [:x | count _ count + 1].
	^count! !

!Behavior methodsFor: 'accessing instances and variables'!
instVarNames
	"Answer an Array of the instance variable names. Behaviors must make
	up fake local instance variable names because Behaviors have instance
	variables for the purpose of compiling methods, but these are not named
	instance variables."

	| mySize superSize |
	mySize _ self instSize.
	superSize _
		superclass == nil
			ifTrue: [0]
			ifFalse: [superclass instSize].
	mySize = superSize ifTrue: [^#()].
	^(superSize + 1 to: mySize) collect: [:i | 'inst' , i printString]! !

!Behavior methodsFor: 'accessing instances and variables'!
sharedPools
	"Answer a Set of the names of the pools (Dictionaries) that the receiver
	shares.
	9/12/96 tk  sharedPools have an order now"

	^ OrderedCollection new! !

!Behavior methodsFor: 'accessing instances and variables'!
someInstance
	"Primitive. Answer the first instance in the enumeration of all instances
	of the receiver. Fails if there are none. Essential. See Object
	documentation whatIsAPrimitive."

	<primitive: 77>
	^nil! !

!Behavior methodsFor: 'accessing instances and variables'!
subclassInstVarNames
	"Answer a Set of the names of the receiver's subclasses' instance
	variables."
	| vars |
	vars _ Set new.
	self allSubclasses do: [:aSubclass | vars addAll: aSubclass instVarNames].
	^vars! !


!Behavior methodsFor: 'testing class hierarchy'!
inheritsFrom: aClass
	"Answer whether the argument, aClass, is on the receiver's superclass
	chain."

	| aSuperclass |
	aSuperclass _ superclass.
	[aSuperclass == nil]
		whileFalse:
			[aSuperclass == aClass ifTrue: [^true].
			aSuperclass _ aSuperclass superclass].
	^false! !

!Behavior methodsFor: 'testing class hierarchy'!
kindOfSubclass
	"Answer a String that is the keyword that describes the receiver's kind of
	subclass, either a regular subclass, a variableSubclass, a
	variableByteSubclass, or a variableWordSubclass."

	self isVariable
		ifTrue: [self isBits
					ifTrue: [self isBytes
								ifTrue: [^' variableByteSubclass: ']
								ifFalse: [^' variableWordSubclass: ']]
					ifFalse: [^' variableSubclass: ']]
		ifFalse: [^' subclass: ']! !


!Behavior methodsFor: 'testing method dictionary'!
allUnsentMessages
	"Answer an array of all the messages defined by the receiver that are not sent anywhere in the system.  5/8/96 sw"

	^ Smalltalk allUnSentMessagesIn: self selectors! !

!Behavior methodsFor: 'testing method dictionary'!
canUnderstand: selector
	"Answer whether the receiver can respond to the message whose selector
	is the argument. The selector can be in the method dictionary of the
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^true].
	superclass == nil ifTrue: [^false].
	^superclass canUnderstand: selector! !

!Behavior methodsFor: 'testing method dictionary' stamp: 'tk 9/13/97 09:53'!
classThatUnderstands: selector
	"Answer the class that can respond to the message whose selector
	is the argument. The selector can be in the method dictionary of the
	receiver's class or any of its superclasses."

	(self includesSelector: selector) ifTrue: [^ self].
	superclass == nil ifTrue: [^ nil].
	^ superclass classThatUnderstands: selector! !

!Behavior methodsFor: 'testing method dictionary'!
hasMethods
	"Answer whether the receiver has any methods in its method dictionary."

	^methodDict size > 0! !

!Behavior methodsFor: 'testing method dictionary'!
includesSelector: aSymbol
	"Answer whether the message whose selector is the argument is in the
	method dictionary of the receiver's class."

	^methodDict includesKey: aSymbol! !

!Behavior methodsFor: 'testing method dictionary'!
scopeHas: name ifTrue: assocBlock
	"If the argument name is a variable known to the receiver, then evaluate
	the second argument, assocBlock."

	^superclass scopeHas: name ifTrue: assocBlock! !

!Behavior methodsFor: 'testing method dictionary' stamp: 'di 9/5/97 16:16'!
thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal.  Dives into the compact literal notation, making it slow but thorough"
	| who method |
	who _ Set new.
	methodDict associationsDo:
		[:assn |
		method _ assn value.
		((method hasLiteralSuchThat: [:lit | lit == literal]) or:
				[specialFlag and: [method scanFor: specialByte]])
			ifTrue:
			[((literal isMemberOf: Association) not
				or: [method sendsToSuper not
					or: [method literals allButLast includes: literal]])
				ifTrue: [who add: assn key]]].
	^ who! !

!Behavior methodsFor: 'testing method dictionary'!
whichClassIncludesSelector: aSymbol
	"Answer the class on the receiver's superclass chain where the argument,
	aSymbol (a message selector), will be found. Answer nil if none found."

	(methodDict includesKey: aSymbol) ifTrue: [^self].
	superclass == nil ifTrue: [^nil].
	^superclass whichClassIncludesSelector: aSymbol

	"Rectangle whichClassIncludesSelector: #inspect."! !

!Behavior methodsFor: 'testing method dictionary'!
whichSelectorsAccess: instVarName
	"Answer a Set of selectors whose methods access the argument,
	instVarName, as a named instance variable."

	| instVarIndex |
	instVarIndex _ self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^methodDict keys select:
		[:sel |
		((methodDict at: sel)
			readsField: instVarIndex)
			or: [(methodDict at: sel) writesField: instVarIndex]]

	"Point whichSelectorsAccess: 'x'."! !

!Behavior methodsFor: 'testing method dictionary'!
whichSelectorsReferTo: literal
	"Answer a Set of selectors whose methods access the argument as a literal."

	| special |
	special _ Smalltalk hasSpecialSelector: literal ifTrueSetByte: [:byte ].
	^self whichSelectorsReferTo: literal special: special byte: byte

	"Rectangle whichSelectorsReferTo: #+."! !

!Behavior methodsFor: 'testing method dictionary' stamp: 'di 10/17/97 22:39'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."
	| who method |
	who _ Set new.
	methodDict associationsDo:
		[:assn |
		method _ assn value.
		((method hasLiteral: literal) or:
				[specialFlag and: [method scanFor: specialByte]])
			ifTrue:
			[((literal isMemberOf: Association) not
				or: [method sendsToSuper not
					or: [method literals allButLast includes: literal]])
				ifTrue: [who add: assn key]]].
	^who! !

!Behavior methodsFor: 'testing method dictionary'!
whichSelectorsStoreInto: instVarName
	"Answer a Set of selectors whose methods access the argument,
	instVarName, as a named instance variable."
	| instVarIndex |
	instVarIndex _ self allInstVarNames indexOf: instVarName ifAbsent: [^Set new].
	^ methodDict keys select:
		[:sel | (methodDict at: sel) writesField: instVarIndex]

	"Point whichSelectorsStoreInto: 'x'."! !


!Behavior methodsFor: 'enumerating'!
allInstancesDo: aBlock
	"Evaluate the argument, aBlock, for each of the current instances of the
	receiver."
	| inst next |
	self ==  UndefinedObject ifTrue: [^ aBlock value: nil].
	inst _ self someInstance.
	[inst == nil]
		whileFalse:
		[aBlock value: inst.
		inst _ inst nextInstance]! !

!Behavior methodsFor: 'enumerating'!
allSubclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's subclasses."

	self subclassesDo:
		[:cl |
		aBlock value: cl.
		cl allSubclassesDo: aBlock]! !

!Behavior methodsFor: 'enumerating' stamp: 'di 6/20/97 10:50'!
allSubInstancesDo: aBlock
	"Evaluate the argument, aBlock, for each of the current instances of the
	receiver and all its subclasses."

	self allInstancesDo: aBlock.
	self allSubclassesDo: [:sub | sub allInstancesDo: aBlock]! !

!Behavior methodsFor: 'enumerating'!
allSuperclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	superclass == nil
		ifFalse: [aBlock value: superclass.
				superclass allSuperclassesDo: aBlock]! !

!Behavior methodsFor: 'enumerating'!
selectSubclasses: aBlock
	"Evaluate the argument, aBlock, with each of the receiver's (next level)
	subclasses as its argument. Collect into a Set only those subclasses for
	which aBlock evaluates to true. In addition, evaluate aBlock for the
	subclasses of each of these successful subclasses and collect into the set
	those for which aBlock evaluates true. Answer the resulting set."

	| aSet |
	aSet _ Set new.
	self allSubclasses do:
		[:aSubclass |
		(aBlock value: aSubclass) ifTrue: [aSet add: aSubclass]].
	^aSet! !

!Behavior methodsFor: 'enumerating'!
selectSuperclasses: aBlock
	"Evaluate the argument, aBlock, with the receiver's superclasses as the
	argument. Collect into an OrderedCollection only those superclasses for
	which aBlock evaluates to true. In addition, evaluate aBlock for the
	superclasses of each of these successful superclasses and collect into the
	OrderedCollection ones for which aBlock evaluates to true. Answer the
	resulting OrderedCollection."

	| aSet |
	aSet _ Set new.
	self allSuperclasses do:
		[:aSuperclass |
		(aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
	^aSet! !

!Behavior methodsFor: 'enumerating'!
subclassesDo: aBlock
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	subclasses == nil ifFalse:
		[subclasses do: [:cl | aBlock value: cl]]! !

!Behavior methodsFor: 'enumerating'!
withAllSubclassesDo: aBlock
	"Evaluate the argument, aBlock, for the receiver and each of its
	subclasses."

	aBlock value: self.
	self allSubclassesDo: aBlock! !


!Behavior methodsFor: 'user interface' stamp: 'sw 8/12/97 20:18'!
allCallsOn: aSymbol
	"Answer a SortedCollection of all the methods that call on aSymbol."

	| aSortedCollection special |
	aSortedCollection _ SortedCollection new.
	special _ Smalltalk hasSpecialSelector: aSymbol ifTrueSetByte: [:byte ].
	self withAllSubclassesDo:
		[:class | (class whichSelectorsReferTo: aSymbol special: special byte: byte) do:
			[:sel | sel ~~ #DoIt ifTrue: [aSortedCollection add: class name , ' ' , sel]]].
	^aSortedCollection! !

!Behavior methodsFor: 'user interface' stamp: 'sw 2/23/98 00:48'!
browse
	Browser newOnClass: self! !

!Behavior methodsFor: 'user interface'!
browseAllAccessesTo: instVarName 	"Collection browseAllAccessesTo: 'contents'."
	"Create and schedule a Message Set browser for all the receiver's methods
	or any methods of a subclass that refer to the instance variable name."
	| coll |
	coll _ OrderedCollection new.
	Cursor wait
		showWhile:
			[self withAllSubclasses do:
				[:class |
				(class whichSelectorsAccess: instVarName) do:
					[:sel | sel ~~ #DoIt ifTrue: [coll add: class name , ' ' , sel]]].
			self allSuperclasses do:
				[:class |
				(class whichSelectorsAccess: instVarName) do:
					[:sel | sel ~~ #DoIt ifTrue: [coll add: class name , ' ' , sel]]]].
	^ Smalltalk browseMessageList: coll name: 'Accesses to ' , instVarName autoSelect: instVarName! !

!Behavior methodsFor: 'user interface'!
browseAllCallsOn: aSymbol
	"Create and schedule a Message Set browser for all the methods that call
	on aSymbol."
	| key label |
	(aSymbol isKindOf: LookupKey)
			ifTrue: [label _ 'Users of ' , (key _ aSymbol key)]
			ifFalse: [label _ 'Senders of ' , (key _ aSymbol)].
	^ Smalltalk browseMessageList: (self allCallsOn: aSymbol) asSortedCollection
		name: label autoSelect: key

	"Number browseAllCallsOn: #/."! !

!Behavior methodsFor: 'user interface'!
browseAllStoresInto: instVarName 	"Collection browseAllStoresInto: 'contents'."
	"Create and schedule a Message Set browser for all the receiver's methods
	or any methods of a subclass that refer to the instance variable name."
	| coll |
	coll _ OrderedCollection new.
	Cursor wait
		showWhile:
			[self withAllSubclasses do:
				[:class |
				(class whichSelectorsStoreInto: instVarName) do:
					[:sel | sel ~~ #DoIt ifTrue: [coll add: class name , ' ' , sel]]].
			self allSuperclasses do:
				[:class |
				(class whichSelectorsStoreInto: instVarName) do:
					[:sel | sel ~~ #DoIt ifTrue: [coll add: class name , ' ' , sel]]]].
	^ Smalltalk browseMessageList: coll name: 'Stores into ' , instVarName autoSelect: instVarName! !

!Behavior methodsFor: 'user interface'!
crossReference
	"Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included."

	^self selectors asSortedCollection asArray collect: [:x | 		Array
			with: (String with: Character cr), x
			with: (self whichSelectorsReferTo: x)]

	"Point crossReference."! !

!Behavior methodsFor: 'user interface' stamp: 'sw 2/4/98 15:21'!
removeUninstantiatedSubclassesSilently
	"Remove the classes of any subclasses that have neither instances nor subclasses.  Answer the number of bytes reclaimed"
	"Player removeUninstantiatedSubclassesSilently"

	| candidatesForRemoval  oldFree |

	oldFree _ Smalltalk garbageCollect.
	candidatesForRemoval _
		self subclasses select: [:c |
			(c instanceCount = 0) and: [c subclasses size = 0]].
	candidatesForRemoval do: [:c | c removeFromSystem].
	^ Smalltalk garbageCollect - oldFree! !

!Behavior methodsFor: 'user interface'!
unreferencedInstanceVariables
	"Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of its subclasses.  2/26/96 sw"

	| any |

	^ self instVarNames copy reject:
		[:ivn | any _ false.
		self withAllSubclasses do:
			[:class |  (class whichSelectorsAccess: ivn) do:
					[:sel | sel ~~ #DoIt ifTrue: [any _ true]]].
		any]

"Ob unreferencedInstanceVariables"! !


!Behavior methodsFor: 'private'!
becomeCompact
	| cct index |
	cct _ Smalltalk compactClassesArray.
	(self indexIfCompact > 0 or: [cct includes: self])
		ifTrue: [^ self halt: self name , 'is already compact'].
	index _ cct indexOf: nil
		ifAbsent: [^ self halt: 'compact class table is full'].
	"Install this class in the compact class table"
	cct at: index put: self.
	"Update instspec so future instances will be compact"
	format _ format + (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Purge any old instances"
	Smalltalk garbageCollect.! !

!Behavior methodsFor: 'private'!
becomeUncompact
	| cct index |
	cct _ Smalltalk compactClassesArray.
	(index _ self indexIfCompact) = 0
		ifTrue: [^ self].
	(cct includes: self)
		ifFalse: [^ self halt  "inconsistent state"].
	"Update instspec so future instances will not be compact"
	format _ format - (index bitShift: 11).
	"Make up new instances and become old ones into them"
	self updateInstancesFrom: self.
	"Make sure there are no compact ones left around"
	Smalltalk garbageCollect.
	"Remove this class from the compact class table"
	cct at: index put: nil.
! !

!Behavior methodsFor: 'private'!
flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed! !

!Behavior methodsFor: 'private'!
format: nInstVars variable: isVar words: isWords pointers: isPointers
	"Set the format for the receiver (a Class)."
	| cClass instSpec sizeHiBits |
	self flag: #instSizeChange.
"
Smalltalk browseAllCallsOn: #instSizeChange.
Smalltalk browseAllImplementorsOf: #fixedFieldsOf:.
Smalltalk browseAllImplementorsOf: #instantiateClass:indexableSize:.
"
"
	NOTE: This code supports the backward-compatible extension to 8 bits of instSize.
	For now the format word is...
		<2 bits=instSize//64><5 bits=cClass><4 bits=instSpec><6 bits=instSize\\64><1 bit=0>
	But when we revise the image format, it should become...
		<5 bits=cClass><4 bits=instSpec><8 bits=instSize><1 bit=0>
"
	sizeHiBits _ (nInstVars+1) // 64.
	cClass _ 0.  "for now"
	instSpec _ isPointers
		ifTrue: [isVar
				ifTrue: [nInstVars>0 ifTrue: [3] ifFalse: [2]]
				ifFalse: [nInstVars>0 ifTrue: [1] ifFalse: [0]]]
		ifFalse: [isWords ifTrue: [6] ifFalse: [8]].
	format _ sizeHiBits.
	format _ (format bitShift: 5) + cClass.
	format _ (format bitShift: 4) + instSpec.
	format _ (format bitShift: 6) + ((nInstVars+1)\\64).  "+1 since prim size field includes header"
	format _ (format bitShift: 1) "This shift plus integer bit lets wordSize work like byteSize"
! !

!Behavior methodsFor: 'private'!
indexIfCompact
	"If these 5 bits are non-zero, then instances of this class
	will be compact.  It is crucial that there be an entry in
	Smalltalk compactClassesArray for any class so optimized.
	See the msgs becomeCompact and becomeUncompact."
	^ (format bitShift: -11) bitAnd: 16r1F
"
Smalltalk compactClassesArray doWithIndex:
	[:c :i | c == nil ifFalse:
		[c indexIfCompact = i ifFalse: [self halt]]]
"! !

!Behavior methodsFor: 'private'!
printSubclassesOn: aStream level: level
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times."

	| subclassNames subclass |
	aStream crtab: level.
	aStream nextPutAll: self name.
	aStream space; print: self instVarNames.
	self == Class
		ifTrue:
			[aStream crtab: level + 1; nextPutAll: '[ ... all the Metaclasses ... ]'.
			^self].
	subclassNames _ self subclasses collect: [:subC | subC name].
	"Print subclasses in alphabetical order"
	subclassNames asSortedCollection do:
		[:name |
		subclass _ self subclasses detect: [:subC | subC name = name].
		subclass printSubclassesOn: aStream level: level + 1]! !

!Behavior methodsFor: 'private' stamp: 'di 12/26/97 11:07'!
removeSelectorSimply: selector
	"Remove the message selector from the receiver's method dictionary.
	Internal access from compiler."

	methodDict removeKey: selector ifAbsent: [^self].
	selector flushCache! !
