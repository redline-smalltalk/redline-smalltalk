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

- allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level
    "Walk the tree of subclasses, giving the class and its level"
    | subclassNames subclass |
    classAndLevelBlock value: self value: level.
    self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
    "Visit subclasses in alphabetical order"
    subclassNames := SortedCollection new.
    self subclassesDo: [:subC | subclassNames add: subC name].
    subclassNames do:
      [:name | (Smalltalk at: name)
        allSubclassesWithLevelDo: classAndLevelBlock
        startingLevel: level+1].

- allSuperclasses
    "Answer an OrderedCollection of the receiver's and the receiver's
    ancestor's superclasses. The first element is the receiver's immediate
    superclass, followed by its superclass; the last element is Object."

    | temp |
    superclass == nil
      ifTrue: [^ OrderedCollection new]
      ifFalse: [temp := superclass allSuperclasses.
          temp addFirst: superclass.
          ^ temp].

- subclasses
    "Answer a Set containing the receiver's subclasses."

    subclasses == nil
      ifTrue: [^ Set new]
      ifFalse: [^ subclasses copy].

- superclass
	"Answer the receiver's superclass, a Class."

	JVM primitive: 306.

- superclass: aClass

  JVM primitive: 307.

- withAllSubclasses
    "Answer a Set of the receiver, the receiver's descendent's, and the
    receiver's descendent's subclasses."

    | aSet |
    aSet := Set with: self.
    aSet addAll: self subclasses.
    self subclasses do: [:eachSubclass | aSet addAll: eachSubclass allSubclasses].
    ^ aSet.

- withAllSuperclasses
    "Answer an OrderedCollection of the receiver and the receiver's
    superclasses. The first element is the receiver,
    followed by its superclass; the last element is Object."

    | temp |
    temp := self allSuperclasses.
    temp addFirst: self.
    ^ temp.


"Behavior methodsFor: 'accessing method dictionary'"

allSelectors
    "Answer a Set of all the message selectors that instances of the receiver
    can understand."

    | temp |
    superclass == nil
      ifTrue: [^ self selectors]
      ifFalse: [temp := superclass allSelectors.
          temp addAll: self selectors.
          ^ temp].

- changeRecordsAt: selector
    "Return a list of ChangeRecords for all versions of the method at selector.
    Source code can be retrieved by sending string to any one"
    "(Pen changeRecordsAt: #go:) collect: [:cRec | cRec string]"

    ^ (ChangeList new
        scanVersionsOf: (self compiledMethodAt: selector)
        class: self meta: self isMeta
        category: (self whichCategoryIncludesSelector: selector)
        selector: selector)
      changeList.

- compiledMethodAt: selector
    "Answer the compiled method associated with the argument, selector (a
    Symbol), a message selector in the receiver's method dictionary. If the
    selector is not in the dictionary, create an error notification."

    ^ methodDict at: selector.

- compiledMethodAt: selector ifAbsent: aBlock
    "Answer the compiled method associated with the argument, selector (a Symbol), a message selector in the receiver's
    method dictionary. If the selector is not in the dictionary, return the value of aBlock"

    ^ methodDict at: selector ifAbsent: [aBlock value].

- compressedSourceCodeAt: selector
    "(Paragraph compressedSourceCodeAt: #displayLines:affectedRectangle:) size 721 1921
    Paragraph selectors inject: 0 into: [:tot :sel | tot + (Paragraph compressedSourceCodeAt: sel) size] 13606 31450"

    | rawText parse |
    rawText := (self sourceCodeAt: selector) asString.
    parse := self compilerClass new parse: rawText in: self notifying: nil.
    ^ rawText compressWithTable:
      ((selector keywords +
      parse tempNames +
      self instVarNames +
      #(self super ifTrue: ifFalse:) +
      ((0 to: 7) collect:
        [:i | String streamContents:
          [:s | s cr. i timesRepeat: [s tab]]]) +
      (self compiledMethodAt: selector) literalStrings)
        asSortedCollection: [:a :b | a size > b size]).

- firstCommentAt:  selector
    "Answer a string representing the first comment in the method associated with selector.  Return an empty string if
    the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart
    enough to bypass quotes in string constants, but ""clever"" enough to map doubled quotes into a single quote."

    | sourceString commentStart  pos nextQuotePos |

    sourceString := self sourceCodeAt: selector.
    sourceString size == 0 ifTrue: [^ ''].
    commentStart := sourceString findString: '"' startingAt: 1.
    commentStart == 0 ifTrue: [^ ''].
    pos := commentStart + 1.
    [(nextQuotePos := sourceString findString: '"' startingAt: pos) == (sourceString findString: '""' startingAt: pos)]
      whileTrue:
        [pos := nextQuotePos + 2].

    ^ (sourceString copyFrom: commentStart + 1 to: nextQuotePos - 1) copyReplaceAll: '""' with: '"'.

- selectorAtMethod: method setClass: classResultBlock
    "Answer both the message selector associated with the compiled method
    and the class in which that selector is defined."

    | sel |
    sel := methodDict keyAtValue: method
          ifAbsent:
            [superclass == nil
              ifTrue:
                [classResultBlock value: self.
                ^ self defaultSelectorForMethod: method].
            sel := superclass selectorAtMethod: method setClass: classResultBlock.
            "Set class to be self, rather than that returned from
            superclass. "
            sel == (self defaultSelectorForMethod: method) ifTrue: [classResultBlock value: self].
            ^ sel].
    classResultBlock value: self.
    ^ sel.

- selectors
    "Answer a Set of all the message selectors specified in the receiver's
    method dictionary."

    ^ methodDict keys.

- selectorsDo: selectorBlock
    "Evaluate selectorBlock for all the message selectors in my method dictionary."

    ^ methodDict keysDo: selectorBlock.

- sourceCodeAt: selector

	^ (methodDict at: selector) getSourceFor: selector in: self.

- sourceCodeAt: selector ifAbsent: aBlock

	^ (methodDict at: selector ifAbsent: [^ aBlock value]) getSourceFor: selector in: self.

- sourceMethodAt: selector
    "Answer the paragraph corresponding to the source code for the
    argument."

    ^ (self sourceCodeAt: selector) asText makeSelectorBoldIn: self.

- sourceMethodAt: selector ifAbsent: aBlock
    "Answer the paragraph corresponding to the source code for the
    argument."

    ^ (self sourceCodeAt: selector ifAbsent: [^ aBlock value]) asText makeSelectorBoldIn: self.

"Behavior methodsFor: 'accessing instances and variables'"

- allClassVarNames
    "Answer a Set of the names of the receiver's and the receiver's ancestor's
    class variables."

    ^ superclass allClassVarNames.

- allInstances
    "Answer a collection of all current instances of the receiver."

    | all |
    all := OrderedCollection new.
    self allInstancesDo: [:x | x == all ifFalse: [all add: x]].
    ^ all asArray.

- allInstVarNames
    "Answer an Array of the names of the receiver's instance variables. The
    Array ordering is the order in which the variables are stored and
    accessed by the interpreter."

    | vars |
    superclass == nil
      ifTrue: [vars := self instVarNames copy]	"Guarantee a copy is answered."
      ifFalse: [vars := superclass allInstVarNames + self instVarNames].
    ^ vars.

- allSharedPools
    "Answer a Set of the names of the pools (Dictionaries) that the receiver
    and the receiver's ancestors share."

    ^ superclass allSharedPools.

- allSubInstances
    "Answer a list of all current instances of the receiver and all of its subclasses."

    | aCollection |
    aCollection := OrderedCollection new.
    self allSubInstancesDo:
      [:x | x == aCollection ifFalse: [aCollection add: x]].
    ^ aCollection.

- classVarNames
    "Answer a Set of the receiver's class variable names."

    ^ Set new.

- inspectAllInstances
    "Inpsect all instances of the receiver.  1/26/96 sw"

    | all allSize prefix |
    all := self allInstances.
    (allSize := all size) == 0 ifTrue: [^ self notify: 'There are no
  instances of ' + self name].
    prefix := allSize == 1
      ifTrue: 	['The lone instance']
      ifFalse:	['The ' + allSize printString + ' instances'].

    all asArray inspectWithLabel: (prefix + ' of ' + self name).

- inspectSubInstances
    "Inspect all instances of the receiver and all its subclasses.  CAUTION - don't do this for something as generic as Object!!  1/26/96 sw"

    | all allSize prefix |
    all := self allSubInstances.
    (allSize := all size) == 0 ifTrue: [^ self notify: 'There are no
  instances of ' + self name + '
  or any of its subclasses'].
    prefix := allSize == 1
      ifTrue: 	['The lone instance']
      ifFalse:	['The ' + allSize printString + ' instances'].

    all asArray inspectWithLabel: (prefix + ' of ' + self name + ' & its subclasses').

- instanceCount
    "Answer the number of instances of the receiver that are currently in
    use."

    | count |
    count := 0.
    self allInstancesDo: [:x | count := count + 1].
    ^ count.

- instVarNames
    "Answer an Array of the instance variable names. Behaviors must make
    up fake local instance variable names because Behaviors have instance
    variables for the purpose of compiling methods, but these are not named
    instance variables."

    | mySize superSize |
    mySize := self instSize.
    superSize :=
      superclass == nil
        ifTrue: [0]
        ifFalse: [superclass instSize].
    mySize = superSize ifTrue: [^ #()].
    ^ (superSize + 1 to: mySize) collect: [:i | 'inst' + i printString].

- sharedPools
    "Answer a Set of the names of the pools (Dictionaries) that the receiver
    shares."

    ^ OrderedCollection new.

- someInstance
    "Primitive. Answer the first instance in the enumeration of all instances
    of the receiver. Fails if there are none. Essential. See Object
    documentation whatIsAPrimitive."

    JVM primitive: 77.

- subclassInstVarNames
    "Answer a Set of the names of the receiver's subclasses' instance
    variables."

    | vars |
    vars := Set new.
    self allSubclasses do: [:aSubclass | vars addAll: aSubclass instVarNames].
    ^ vars.

"Behavior methodsFor: 'testing class hierarchy'"

- inheritsFrom: aClass
    "Answer whether the argument, aClass, is on the receiver's superclass
    chain."

    | aSuperclass |
    aSuperclass := superclass.
    [aSuperclass == nil]
      whileFalse:
        [aSuperclass == aClass ifTrue: [^ true].
        aSuperclass := aSuperclass superclass].
    ^ false.

- kindOfSubclass
    "Answer a String that is the keyword that describes the receiver's kind of
    subclass, either a regular subclass, a variableSubclass, a
    variableByteSubclass, or a variableWordSubclass."

    self isVariable
      ifTrue: [self isBits
            ifTrue: [self isBytes
                  ifTrue: [^ ' variableByteSubclass: ']
                  ifFalse: [^ ' variableWordSubclass: ']]
            ifFalse: [^ ' variableSubclass: ']]
      ifFalse: [^ ' subclass: '].

- allUnsentMessages
    "Answer an array of all the messages defined by the receiver that are not sent anywhere in the system."

    ^ Smalltalk allUnSentMessagesIn: self selectors.

"Behavior methodsFor: 'testing method dictionary'"

- canUnderstand: selector
    "Answer whether the receiver can respond to the message whose selector
    is the argument. The selector can be in the method dictionary of the
    receiver's class or any of its superclasses."

    (self includesSelector: selector) ifTrue: [^ true].
    superclass == nil ifTrue: [^ false].
    ^ superclass canUnderstand: selector.

- classThatUnderstands: selector
    "Answer the class that can respond to the message whose selector
    is the argument. The selector can be in the method dictionary of the
    receiver's class or any of its superclasses."

    (self includesSelector: selector) ifTrue: [^ self].
    superclass == nil ifTrue: [^ nil].
    ^ superclass classThatUnderstands: selector.

- hasMethods
    "Answer whether the receiver has any methods in its method dictionary."

    ^ methodDict size > 0.

- includesSelector: aSymbol
    "Answer whether the message whose selector is the argument is in the
    method dictionary of the receiver's class."

    ^ methodDict includesKey: aSymbol.

- scopeHas: name ifTrue: assocBlock
    "If the argument name is a variable known to the receiver, then evaluate
    the second argument, assocBlock."

    ^ superclass scopeHas: name ifTrue: assocBlock.

- thoroughWhichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal.  Dives into the compact literal notation, making it slow but thorough"

	| who method |
	who := Set new.
	methodDict associationsDo:
		[:assn |
		method := assn value.
		((method hasLiteralSuchThat: [:lit | lit == literal]) or:
				[specialFlag and: [method scanFor: specialByte]])
			ifTrue:
			[((literal isMemberOf: Association) not
				or: [method sendsToSuper not
					or: [method literals allButLast includes: literal]])
				ifTrue: [who add: assn key]]].
	^ who.

- whichClassIncludesSelector: aSymbol
    "Answer the class on the receiver's superclass chain where the argument,
    aSymbol (a message selector), will be found. Answer nil if none found."

    (methodDict includesKey: aSymbol) ifTrue: [^ self].
    superclass == nil ifTrue: [^ nil].
    ^ superclass whichClassIncludesSelector: aSymbol.

- whichSelectorsAccess: instVarName
    "Answer a Set of selectors whose methods access the argument,
    instVarName, as a named instance variable."

    | instVarIndex |
    instVarIndex := self allInstVarNames indexOf: instVarName ifAbsent: [^ Set new].
    ^ methodDict keys select:
      [:sel |
      ((methodDict at: sel)
        readsField: instVarIndex)
        or: [(methodDict at: sel) writesField: instVarIndex]].

- whichSelectorsReferTo: literal
    "Answer a Set of selectors whose methods access the argument as a literal."

    | special |
    special := Smalltalk hasSpecialSelector: literal ifTrueSetByte: [:byte ].
    ^ self whichSelectorsReferTo: literal special: special byte: byte.

- whichSelectorsReferTo: literal special: specialFlag byte: specialByte
    "Answer a set of selectors whose methods access the argument as a literal."

    | who method |
    who := Set new.
    methodDict associationsDo:
      [:assn |
      method := assn value.
      ((method hasLiteral: literal) or:
          [specialFlag and: [method scanFor: specialByte]])
        ifTrue:
        [((literal isMemberOf: Association) not
          or: [method sendsToSuper not
            or: [method literals allButLast includes: literal]])
          ifTrue: [who add: assn key]]].
    ^ who.

- whichSelectorsStoreInto: instVarName
    "Answer a Set of selectors whose methods access the argument,
    instVarName, as a named instance variable."

    | instVarIndex |
    instVarIndex := self allInstVarNames indexOf: instVarName ifAbsent: [^ Set new].
    ^ methodDict keys select:
      [:sel | (methodDict at: sel) writesField: instVarIndex].

"Behavior methodsFor: 'enumerating'"

- allInstancesDo: aBlock
    "Evaluate the argument, aBlock, for each of the current instances of the
    receiver."
    | inst next |
    self ==  UndefinedObject ifTrue: [^ aBlock value: nil].
    inst := self someInstance.
    [inst == nil]
      whileFalse:
      [aBlock value: inst.
      inst := inst nextInstance].

- allSubclassesDo: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's subclasses."

    self subclassesDo:
      [:cl |
      aBlock value: cl.
      cl allSubclassesDo: aBlock].

- allSubInstancesDo: aBlock
    "Evaluate the argument, aBlock, for each of the current instances of the
    receiver and all its subclasses."

    self allInstancesDo: aBlock.
    self allSubclassesDo: [:sub | sub allInstancesDo: aBlock].

- allSuperclassesDo: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's superclasses."

    superclass == nil
      ifFalse: [aBlock value: superclass.
          superclass allSuperclassesDo: aBlock].

- selectSubclasses: aBlock
    "Evaluate the argument, aBlock, with each of the receiver's (next level)
    subclasses as its argument. Collect into a Set only those subclasses for
    which aBlock evaluates to true. In addition, evaluate aBlock for the
    subclasses of each of these successful subclasses and collect into the set
    those for which aBlock evaluates true. Answer the resulting set."

    | aSet |
    aSet := Set new.
    self allSubclasses do:
      [:aSubclass |
      (aBlock value: aSubclass) ifTrue: [aSet add: aSubclass]].
    ^ aSet.

- selectSuperclasses: aBlock
    "Evaluate the argument, aBlock, with the receiver's superclasses as the
    argument. Collect into an OrderedCollection only those superclasses for
    which aBlock evaluates to true. In addition, evaluate aBlock for the
    superclasses of each of these successful superclasses and collect into the
    OrderedCollection ones for which aBlock evaluates to true. Answer the
    resulting OrderedCollection."

    | aSet |
    aSet := Set new.
    self allSuperclasses do:
      [:aSuperclass |
      (aBlock value: aSuperclass) ifTrue: [aSet add: aSuperclass]].
    ^ aSet.

- subclassesDo: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."

    subclasses == nil ifFalse:
      [subclasses do: [:cl | aBlock value: cl]].

- withAllSubclassesDo: aBlock
    "Evaluate the argument, aBlock, for the receiver and each of its
    subclasses."

    aBlock value: self.
    self allSubclassesDo: aBlock.

"Behavior methodsFor: 'user interface'"

- allCallsOn: aSymbol
    "Answer a SortedCollection of all the methods that call on aSymbol."

    | aSortedCollection special |
    aSortedCollection := SortedCollection new.
    special := Smalltalk hasSpecialSelector: aSymbol ifTrueSetByte: [:byte ].
    self withAllSubclassesDo:
      [:class | (class whichSelectorsReferTo: aSymbol special: special byte: byte) do:
        [:sel | sel ~~ #DoIt ifTrue: [aSortedCollection add: class name + ' ' + sel]]].
    ^ aSortedCollection.

- browse
	Browser newOnClass: self.

- browseAllAccessesTo: instVarName 	"Collection browseAllAccessesTo: 'contents'."
    "Create and schedule a Message Set browser for all the receiver's methods
    or any methods of a subclass that refer to the instance variable name."

    | coll |
    coll := OrderedCollection new.
    Cursor wait
      showWhile:
        [self withAllSubclasses do:
          [:class |
          (class whichSelectorsAccess: instVarName) do:
            [:sel | sel ~~ #DoIt ifTrue: [coll add: class name + ' ' + sel]]].
        self allSuperclasses do:
          [:class |
          (class whichSelectorsAccess: instVarName) do:
            [:sel | sel ~~ #DoIt ifTrue: [coll add: class name + ' ' + sel]]]].
    ^ Smalltalk browseMessageList: coll name: 'Accesses to ' + instVarName autoSelect: instVarName.

- browseAllCallsOn: aSymbol
    "Create and schedule a Message Set browser for all the methods that call
    on aSymbol."

    | key label |
    (aSymbol isKindOf: LookupKey)
        ifTrue: [label := 'Users of ' + (key := aSymbol key)]
        ifFalse: [label := 'Senders of ' + (key := aSymbol)].
    ^ Smalltalk browseMessageList: (self allCallsOn: aSymbol) asSortedCollection
      name: label autoSelect: key.

- browseAllStoresInto: instVarName 	"Collection browseAllStoresInto: 'contents'."
    "Create and schedule a Message Set browser for all the receiver's methods
    or any methods of a subclass that refer to the instance variable name."

    | coll |
    coll := OrderedCollection new.
    Cursor wait
      showWhile:
        [self withAllSubclasses do:
          [:class |
          (class whichSelectorsStoreInto: instVarName) do:
            [:sel | sel ~~ #DoIt ifTrue: [coll add: class name + ' ' + sel]]].
        self allSuperclasses do:
          [:class |
          (class whichSelectorsStoreInto: instVarName) do:
            [:sel | sel ~~ #DoIt ifTrue: [coll add: class name + ' ' + sel]]]].
    ^ Smalltalk browseMessageList: coll name: 'Stores into ' + instVarName autoSelect: instVarName.

- crossReference
    "Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary
    and whose second element is a set of all message selectors in the method dictionary whose methods send a message
    with that selector. Subclasses are not included."

    ^ self selectors asSortedCollection asArray collect: [:x | 		Array
        with: (String with: Character cr) + x
        with: (self whichSelectorsReferTo: x)].


- removeUninstantiatedSubclassesSilently
    "Remove the classes of any subclasses that have neither instances nor subclasses.  Answer the number of bytes reclaimed"
    "Player removeUninstantiatedSubclassesSilently"

    | candidatesForRemoval  oldFree |

    oldFree := Smalltalk garbageCollect.
    candidatesForRemoval :=
      self subclasses select: [:c |
        (c instanceCount = 0) and: [c subclasses size = 0]].
    candidatesForRemoval do: [:c | c removeFromSystem].
    ^ Smalltalk garbageCollect - oldFree.

- unreferencedInstanceVariables
    "Return a list of the instance variables defined in the receiver which are not referenced in the receiver or any of
     its subclasses."

    | any |

    ^ self instVarNames copy reject:
      [:ivn | any := false.
      self withAllSubclasses do:
        [:class |  (class whichSelectorsAccess: ivn) do:
            [:sel | sel ~~ #DoIt ifTrue: [any := true]]].
      any].


"Behavior methodsFor: 'private'"

- becomeCompact
    | cct index |
    cct := Smalltalk compactClassesArray.
    (self indexIfCompact > 0 or: [cct includes: self])
      ifTrue: [^ self halt: self name + 'is already compact'].
    index := cct indexOf: nil
      ifAbsent: [^ self halt: 'compact class table is full'].
    "Install this class in the compact class table"
    cct at: index put: self.
    "Update instspec so future instances will be compact"
    format := format + (index bitShift: 11).
    "Make up new instances and become old ones into them"
    self updateInstancesFrom: self.
    "Purge any old instances"
    Smalltalk garbageCollect.

- becomeUncompact
    | cct index |
    cct := Smalltalk compactClassesArray.
    (index := self indexIfCompact) = 0
      ifTrue: [^ self].
    (cct includes: self)
      ifFalse: [^ self halt  "inconsistent state"].
    "Update instspec so future instances will not be compact"
    format := format - (index bitShift: 11).
    "Make up new instances and become old ones into them"
    self updateInstancesFrom: self.
    "Make sure there are no compact ones left around"
    Smalltalk garbageCollect.
    "Remove this class from the compact class table"
    cct at: index put: nil.

- flushCache
    "Tell the interpreter to remove the contents of its method lookup cache, if it has
    one.  Essential.  See Object documentation whatIsAPrimitive."

    JVM primitive: 89.

- format: nInstVars variable: isVar words: isWords pointers: isPointers
    "Set the format for the receiver (a Class)."

    | cClass instSpec sizeHiBits |
    self flag: #instSizeChange.
    sizeHiBits := (nInstVars+1) // 64.
    cClass := 0.  "for now"
    instSpec := isPointers
      ifTrue: [isVar
          ifTrue: [nInstVars>0 ifTrue: [3] ifFalse: [2]]
          ifFalse: [nInstVars>0 ifTrue: [1] ifFalse: [0]]]
      ifFalse: [isWords ifTrue: [6] ifFalse: [8]].
    format := sizeHiBits.
    format := (format bitShift: 5) + cClass.
    format := (format bitShift: 4) + instSpec.
    format := (format bitShift: 6) + ((nInstVars+1)\\64).  "+1 since prim size field includes header"
    format := (format bitShift: 1). "This shift plus integer bit lets wordSize work like byteSize"

- indexIfCompact
    "If these 5 bits are non-zero, then instances of this class
    will be compact.  It is crucial that there be an entry in
    Smalltalk compactClassesArray for any class so optimized.
    See the msgs becomeCompact and becomeUncompact."
    ^ (format bitShift: -11) bitAnd: 16r1F.

- printSubclassesOn: aStream level: level
    "As part of the algorithm for printing a description of the receiver, print the
    subclass on the file stream, aStream, indenting level times."

    | subclassNames subclass |
    aStream crtab: level.
    aStream nextPutAll: self name.
    aStream space; print: self instVarNames.
    self == Class
      ifTrue:
        [aStream crtab: level + 1; nextPutAll: '[ ... all the Metaclasses ... ]'.
        ^ self].
    subclassNames := self subclasses collect: [:subC | subC name].
    "Print subclasses in alphabetical order"
    subclassNames asSortedCollection do:
      [:name |
      subclass := self subclasses detect: [:subC | subC name = name].
      subclass printSubclassesOn: aStream level: level + 1].

- removeSelectorSimply: selector
    "Remove the message selector from the receiver's method dictionary.
    Internal access from compiler."

    methodDict removeKey: selector ifAbsent: [^ self].
    selector flushCache.
