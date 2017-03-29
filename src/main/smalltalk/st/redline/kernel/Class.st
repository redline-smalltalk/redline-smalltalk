"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"My instances describe the representation and behavior of objects. I add more comprehensive programming support
 facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.
 An example is accessing shared (pool) variables."

ClassDescription subclass: #Class
                 instanceVariableNames: 'name classPool sharedPools '
                 category: 'Kernel-Classes'.

"Class methodsFor: 'initialize-release'"

- declare: varString
    "Declare class variables common to all instances. Answer whether
    recompilation is advisable."

    | newVars conflicts assoc class |
    newVars :=
      (Scanner new scanFieldNames: varString)
        collect: [:x | x asSymbol].
    newVars do:
      [:var | var first isLowercase
        ifTrue: [self error: var + ' class variable name should be capitalized; proceed to include anyway.']].
    conflicts := false.
    classPool == nil
      ifFalse: [(classPool keys reject: [:x | newVars includes: x]) do:
            [:var | self removeClassVarName: var]].
    (newVars reject: [:var | self classPool includesKey: var])
      do: [:var | "adding"
        "check if new vars defined elsewhere"
        (self scopeHas: var ifTrue: [:ignored | ignored])
          ifTrue:
            [self error: var + ' is defined elsewhere'.
            conflicts := true]].
    newVars size > 0
      ifTrue:
        [classPool := self classPool.
        "in case it was nil"
        newVars do: [:var | classPool declare: var from: Undeclared]].
    ^ conflicts.

- obsolete
    "Change the receiver to an obsolete class by changing its name to have
    the prefix -AnObsolete-."

    name := 'AnObsolete' + name.
    classPool := Dictionary new.
    self class obsolete.
    super obsolete.

- removeFromSystem
    "Forget the receiver from the Smalltalk global dictionary. Any existing
    instances will refer to an obsolete version of the receiver."

    Smalltalk removeClassFromSystem: self.
    self obsolete.

- sharing: poolString
    "Set up sharedPools. Answer whether recompilation is advisable."

    | oldPools found |
    oldPools := self sharedPools.
    sharedPools := OrderedCollection new.
    (Scanner new scanFieldNames: poolString) do:
      [:poolName |
      sharedPools add: (Smalltalk at: poolName asSymbol)].
    sharedPools isEmpty ifTrue: [sharedPools := nil].
    oldPools do: [:pool | found := false.
          self sharedPools do: [:p | p == pool ifTrue: [found := true]].
          found ifFalse: [^ true "A pool got deleted"]].
    ^ false.

- superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet
    "Answer an instance of me, a new class, using the arguments of the
    message as the needed information."

    superclass := sup.
    methodDict := md.
    format := ft.
    name := nm.
    organization := org.
    instanceVariables := nilOrArray.
    classPool := pool.
    sharedPools := poolSet.

- validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods
    "Recompile the receiver and redefine its subclasses if necessary."

    super
      validateFrom: oldClass
      in: environ
      instanceVariableNames: invalidFields
      methods: invalidMethods.
    self ~~ oldClass
      ifTrue:
        [environ at: name put: self.
        oldClass obsolete].


"Class methodsFor: 'accessing'"

- classPool
    "Answer the dictionary of class variables."

    classPool == nil
      ifTrue: [^ Dictionary new]
      ifFalse: [^ classPool].

- name
    "Answer the name of the receiver."

    name == nil
      ifTrue: [^ super name]
      ifFalse: [^ name].


"Class methodsFor: 'testing'"

- hasMethods
    "Answer a Boolean according to whether any methods are defined for the
    receiver (includes whether there are methods defined in the receiver's
    metaclass)."

    ^ super hasMethods or: [self class hasMethods].

"Class methodsFor: 'copying'"

- copy
    | newClass |
    newClass := self class copy new
      superclass: superclass
      methodDict: methodDict copy
      format: format
      name: name
      organization: organization copy
      instVarNames: instanceVariables copy
      classPool: classPool copy
      sharedPools: sharedPools.
    Class instSize+1 to: self class instSize do:
      [:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
    ^ newClass.

- copyForValidation
    "Make a copy of the receiver (a class) but do not install the created class
    as a new class in the system. This is used for creating a new version of
    the receiver in which the installation is deferred until all changes are
    successfully completed."

    | newClass |
    newClass := self class copy new
      superclass: superclass
      methodDict: methodDict copy
      format: format
      name: name
      organization: organization
      instVarNames: instanceVariables copy
      classPool: classPool
      sharedPools: sharedPools.
    Class instSize+1 to: self class instSize do:
      [:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
    ^ newClass.

- copyOfMethodDictionary
    "Return a copy of the receiver's method dictionary.  "

    ^ methodDict copy.


"Class methodsFor: 'class name'"

- rename: aString
    "The new name of the receiver is the argument, aString."
    self error: 'Cannot rename a class.'.

"Class methodsFor: 'instance variables'"

- addInstVarName: aString
    "Add the argument, aString, as one of the receiver's instance variables."

    superclass class
      name: self name
      inEnvironment: Smalltalk
      subclassOf: superclass
      instanceVariableNames: self instanceVariablesString + aString
      variable: self isVariable
      words: self isWords
      pointers: self isPointers
      classVariableNames: self classVariablesString
      poolDictionaries: self sharedPoolsString
      category: self category
      comment: nil
      changed: false.

- removeInstVarName: aString
    "Remove the argument, aString, as one of the receiver's instance variables."

    | newInstVarString |
    (self instVarNames includes: aString)
      ifFalse: [self error: aString + ' is not one of my instance variables'].
    newInstVarString := ''.
    (self instVarNames copyWithout: aString) do:
      [:varName | newInstVarString := newInstVarString + ' ' + varName].
    superclass class
      name: self name
      inEnvironment: Smalltalk
      subclassOf: superclass
      instanceVariableNames: newInstVarString
      variable: self isVariable
      words: self isWords
      pointers: self isPointers
      classVariableNames: self classVariablesString
      poolDictionaries: self sharedPoolsString
      category: self category
      comment: nil
      changed: false.


"Class methodsFor: 'class variables'"

- addClassVarName: aString
    "Add the argument, aString, as a class variable of the receiver.
    Signal an error if the first character of aString is not capitalized,
    or if it is already a variable named in the class."

    | symbol index |
    aString first isLowercase
      ifTrue: [^ self error: aString + ' class variable name should be capitalized; proceed to include anyway.'].
    symbol := aString asSymbol.
    self withAllSubclasses do:
      [:subclass |
      subclass scopeHas: symbol
        ifTrue: [:temp |
            ^ self error: aString
              + ' is already used as a variable name in class '
              + subclass name]].
    classPool == nil ifTrue: [classPool := Dictionary new].
    (classPool includesKey: symbol) ifFalse:
      ["Pick up any refs in Undeclared"
      classPool declare: symbol from: Undeclared.
      Smalltalk changes changeClass: self].

- allClassVarNames
    "Answer a Set of the names of the receiver's class variables, including those
    defined in the superclasses of the receiver."

    | aSet |
    superclass == nil
      ifTrue:
        [^ self classVarNames]  "This is the keys so it is a new Set."
      ifFalse:
        [aSet := superclass allClassVarNames.
        aSet addAll: self classVarNames.
        ^ aSet].

- classVarNames
    "Answer a Set of the names of the class variables defined in the receiver."

    ^ self classPool keys.

- ensureClassPool

    classPool ifNil: [classPool := Dictionary new].

- initialize
    "Typically used for the initialization of class variables and metaclass
    instance variables. Does nothing, but may be overridden in Metaclasses."

    ^ self.

- removeClassVarName: aString
    "Remove the class variable whose name is the argument, aString, from
    the names defined in the receiver, a class. Create an error notification if
    aString is not a class variable or if it is still being used in the code of
    the class."

    | anAssoc aSymbol |
    aSymbol := aString asSymbol.
    (classPool includesKey: aSymbol)
      ifFalse: [^ self error: aString + ' is not a class variable'].
    anAssoc := classPool associationAt: aSymbol.
    self withAllSubclasses do:
      [:subclass |
      (Array with: subclass with: subclass class) do:
        [:classOrMeta |
        (classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
          isEmpty
            ifFalse: [^ self error: aString
                  + ' is still used in code of class '
                  + classOrMeta name]]].
    classPool removeKey: aSymbol.


"Class methodsFor: 'pool variables'"

- addSharedPool: aDictionary
    "Add the argument, aDictionary, as one of the receiver's pool dictionaries.
    Create an error if the dictionary is already one of the pools."

    (self sharedPools includes: aDictionary)
      ifTrue: [^ self error: 'The dictionary is already in my pool'].
    sharedPools == nil
      ifTrue: [sharedPools := OrderedCollection with: aDictionary]
      ifFalse: [sharedPools add: aDictionary].

- allSharedPools
    "Answer a Set of the pools the receiver shares, including those defined
    in the superclasses of the receiver."

    | aSet |
    superclass == nil
      ifTrue:
        [^ self sharedPools copy]
      ifFalse:
        [aSet := superclass allSharedPools.
        aSet addAll: self sharedPools.
        ^ aSet].

- removeSharedPool: aDictionary
    "Remove the pool dictionary, aDictionary, as one of the receiver's pool
    dictionaries. Create an error notification if the dictionary is not one of
    the pools.
    : Note that it removes the wrong one if there are two empty Dictionaries in the list."

    | satisfiedSet workingSet aSubclass |
    (self sharedPools includes: aDictionary)
      ifFalse: [^ self error: 'the dictionary is not in my pool'].

    "first see if it is declared in a superclass in which case we can remove it."
    (self selectSuperclasses: [:class | class sharedPools includes: aDictionary]) isEmpty
      ifFalse: [sharedPools remove: aDictionary.
          sharedPools isEmpty ifTrue: [sharedPools := nil].
          ^ self].

    "second get all the subclasses that reference aDictionary through me rather than a
    superclass that is one of my subclasses."

    workingSet := self subclasses asOrderedCollection.
    satisfiedSet := Set new.
    [workingSet isEmpty] whileFalse:
      [aSubclass := workingSet removeFirst.
      (aSubclass sharedPools includes: aDictionary)
        ifFalse:
          [satisfiedSet add: aSubclass.
          workingSet addAll: aSubclass subclasses]].

    "for each of these, see if they refer to any of the variables in aDictionary because
    if they do, we can not remove the dictionary."
    satisfiedSet add: self.
    satisfiedSet do:
      [:sub |
      aDictionary associationsDo:
        [:aGlobal |
        (sub whichSelectorsReferTo: aGlobal) isEmpty
          ifFalse: [^ self error: aGlobal key
                  + ' is still used in code of class '
                  + sub name]]].
    sharedPools remove: aDictionary.
    sharedPools isEmpty ifTrue: [sharedPools := nil].

- sharedPools
    "Answer a Set of the pool dictionaries declared in the receiver."

    sharedPools == nil
      ifTrue: [^ OrderedCollection new]
      ifFalse: [^ sharedPools].


"Class methodsFor: 'compiling'"

- compileAllFrom: oldClass
    "Recompile all the methods in the receiver's method dictionary (not the
    subclasses). Also recompile the methods in the metaclass."

    super compileAllFrom: oldClass.
    self class compileAllFrom: oldClass class.

"Class methodsFor: 'compiling'"

- possibleVariablesFor: misspelled continuedFrom: oldResults

    | results |
    results := misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
    self sharedPools do: [:pool |
      results := misspelled correctAgainstDictionary: pool continuedFrom: results ].
    superclass == nil
      ifTrue:
        [ ^ misspelled correctAgainstDictionary: Smalltalk continuedFrom: results ]
      ifFalse:
        [ ^ superclass possibleVariablesFor: misspelled continuedFrom: results ].

- scopeHas: varName ifTrue: assocBlock
    "Look up the first argument, varName, in the context of the receiver. If it is there,
    pass the association to the second argument, assocBlock, and answer true.
    Else answer false.
    : Allow key in shared pools to be a string for HyperSqueak"

    | assoc |
    assoc := self classPool associationAt: varName ifAbsent: [].
    assoc == nil
      ifFalse:
        [assocBlock value: assoc.
        ^ true].
    self sharedPools do:
      [:pool |
      varName = #Textual ifTrue: [self halt].
      assoc := pool associationAt: varName ifAbsent: [
        pool associationAt: varName asString ifAbsent: []].
      assoc == nil
        ifFalse:
          [assocBlock value: assoc.
          ^true]].
    superclass == nil
      ifTrue:
        [assoc := Smalltalk associationAt: varName ifAbsent: [].
        assoc == nil
          ifFalse:
            [assocBlock value: assoc.
            ^ true].
        ^ false].
    ^ superclass scopeHas: varName ifTrue: assocBlock.


"Class methodsFor: 'subclass creation'"

- newSubclass
	| i className |
	i := 1.
	[className := (self name + i printString) asSymbol.
	 Smalltalk includesKey: className]
		whileTrue: [i := i + 1].

	^ self subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'UserObjects'.

- subclass: className
    "Shorthand for subclass creation."
    ^ self subclass: className instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: ''.

- subclass: className  category: cat
    "Shorthand for subclass creation."
    ^ self subclass: className instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: cat.

- subclass: className instanceVariableNames: ivn category: cat
    "Shorthand for subclass creation."
    ^ self subclass: className instanceVariableNames: ivn classVariableNames: '' poolDictionaries: '' category: cat.

- subclass: className instanceVariableNames: ivn classVariableNames: cvn poolDictionaries: pd category: cat
    "This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver)."
    JVM aload: 1;
        aload: 2;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveSubclass' matching: '(Lst/redline/core/PrimContext;)Lst/redline/core/PrimObject;'.

- variableByteSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
    "This is the standard initialization message for creating a new class as a
    subclass of an existing class (the receiver) in which the subclass is to
    have indexable byte-sized nonpointer variables."

    self instSize > 0
      ifTrue: [^ self error: 'cannot make a byte subclass of a class with named fields'].
    (self isVariable and: [self isWords])
      ifTrue: [^ self error: 'cannot make a byte subclass of a class with word fields'].
    (self isVariable and: [self isPointers])
      ifTrue: [^ self error:
            'cannot make a byte subclass of a class with pointer fields'].
    ^ self class name: t
      inEnvironment: Smalltalk
      subclassOf: self
      instanceVariableNames: f
      variable: true
      words: false
      pointers: false
      classVariableNames: d
      poolDictionaries: s
      category: cat
      comment: nil
      changed: false.

- variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
    "This is the standard initialization message for creating a new class as a
    subclass of an existing class (the receiver) in which the subclass is to
    have indexable pointer variables."

    self isBits
      ifTrue:
        [^s elf error:
          'cannot make a pointer subclass of a class with non-pointer fields'].
    ^ self class name: t
      inEnvironment: Smalltalk
      subclassOf: self
      instanceVariableNames: f
      variable: true
      words: true
      pointers: true
      classVariableNames: d
      poolDictionaries: s
      category: cat
      comment: nil
      changed: false.

- variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
    "This is the standard initialization message for creating a new class as a
    subclass of an existing class (the receiver) in which the subclass is to
    have indexable word-sized nonpointer variables."

    self instSize > 0
      ifTrue: [^ self error:
            'cannot make a word subclass of a class with named fields'].
    self isBytes
      ifTrue: [^ self error: 'cannot make a word subclass of a class with byte fields'].
    (self isVariable and: [self isPointers])
      ifTrue: [^ self error:
            'cannot make a word subclass of a class with pointer fields'].
    ^ self class name: t
      inEnvironment: Smalltalk
      subclassOf: self
      instanceVariableNames: f
      variable: true
      words: true
      pointers: false
      classVariableNames: d
      poolDictionaries: s
      category: cat
      comment: nil
      changed: false.


"Class methodsFor: 'fileIn/Out'"

- fileOut
    "Create a file whose name is the name of the receiver with '.st' as the
    extension, and file a description of the receiver onto it."
    ^ self fileOutAsHtml: false.

"Class methodsFor: 'fileIn/Out'"

- fileOutAsHtml: useHtml
    "File a description of the receiver onto a new file whose base name is the name of the receiver."

    | fileStream |
    fileStream := useHtml
      ifTrue: [(FileStream newFileNamed: self name + FileDirectory dot + 'html') asHtml]
      ifFalse: [FileStream newFileNamed: self name + FileDirectory dot + 'st'].
    fileStream header; timeStamp.
    self sharedPools size > 0 ifTrue: [
      self shouldFileOutPools
        ifTrue: [self fileOutSharedPoolsOn: fileStream]].
    self fileOutOn: fileStream moveSource: false toFile: 0.
    fileStream trailer; close.

- fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
    "File a description of the receiver on aFileStream. If the boolean argument,
    moveSource, is true, then set the trailing bytes to the position of aFileStream and
    to fileIndex in order to indicate where to find the source code."

    Transcript cr; show: name.
    super
      fileOutOn: aFileStream
      moveSource: moveSource
      toFile: fileIndex.
    self class nonTrivial
      ifTrue:
        [aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; cr; cr.
        self class
          fileOutOn: aFileStream
          moveSource: moveSource
          toFile: fileIndex].

- fileOutPool: aPool onFileStream: aFileStream
    | aPoolName aValue |
    aPoolName := Smalltalk keyAtValue: aPool.
    Transcript cr; show: aPoolName.
    aFileStream nextPutAll: 'Transcript show: ''' + aPoolName + '''; cr!!'; cr.
    aFileStream nextPutAll: 'Smalltalk at: #' + aPoolName + ' put: Dictionary new!!'; cr.
    aPool keys asSortedCollection do: [ :aKey |
      aValue := aPool at: aKey.
      aFileStream nextPutAll: aPoolName + ' at: #' + aKey asString + ' put:  '.
      (aValue isKindOf: Number)
        ifTrue: [aValue printOn: aFileStream]
        ifFalse: [aFileStream nextPutAll: '('.
            aValue printOn: aFileStream.
            aFileStream nextPutAll: ')'].
      aFileStream nextPutAll: '!!'; cr].
    aFileStream cr.

- fileOutSharedPoolsOn: aFileStream
    "file out the shared pools of this class after prompting the user about each pool"
    | poolsToFileOut |
    poolsToFileOut := self sharedPools select:
      [:aPool | (self shouldFileOutPool: (Smalltalk keyAtValue: aPool))].
    poolsToFileOut do: [:aPool | self fileOutPool: aPool onFileStream: aFileStream].

- objectToStoreOnDataStream
    "I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."

    ^ DiskProxy global: self theNonMetaClass name selector: #yourself
        args: (Array new).

- reformatAll
    "Reformat all methods in this class.
    Leaves old code accessible to version browsing"
    super reformatAll.		"me..."
    self class reformatAll.	"...and my metaclass"

- removeFromChanges
    "References to the receiver, a class, and its metaclass should no longer be included in the system ChangeSet."

    Smalltalk changes removeClassAndMetaClassChanges: self.

- shouldFileOutPool: aPoolName
    "respond with true if the user wants to file out aPoolName"
    ^ self confirm: ('FileOut the sharedPool '+ aPoolName + '?').

- shouldFileOutPools
    "respond with true if the user wants to file out the shared pools"
    ^ self confirm: 'FileOut selected sharedPools?'.

- storeDataOn: aDataStream
    "I don't get stored.  Use a DiskProxy"

    self error: 'use a DiskProxy to store a Class'.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Class class methodsFor: 'instance creation'"

+ template: category
    "Answer an expression that can be edited and evaluated in order to
    define a new class."

    ^ 'Object subclass: #NameOfClass
    instanceVariableNames: ''instVarName1 instVarName2''
    classVariableNames: ''ClassVarName1 ClassVarName2''
    poolDictionaries: ''''
    category: ''' + category + ''''.

+ fileOutPool: aString
    "file out the global pool named aString"
    | f |
    f := FileStream newFileNamed: aString + '.st'.
    self new fileOutPool: (Smalltalk at: aString asSymbol) onFileStream: f.
    f close.
