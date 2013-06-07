"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Behavior.

"category: creating method dictionary"

- methodDictionary: aDictionary
    "Store the argument, aDictionary, as the method dictionary of the receiver."
    self notImplementedYet.

- addSelector: selector withMethod: compiledMethod
    "Add the message selector, selector, with the corresponding compiled method, compiled-Method, to the receiver's
     method dictionary."
    self notImplementedYet.

- removeSelector: selector
    "Remove the argument, selector (which is a Symbol representing a message selector), from the receiver's method
     dictionary. If the selector is not in the method dictionary, report an error."
    self notImplementedYet.

- compile: code
    "The argument, code, is either a String or an object that converts to a String or it is a PositionableStream
     accessing an object that is or converts to a String. Compile code as the source code in the context of the
     receiver's variables. Report an error if the code can not be compiled."
    self notImplementedYet.

- compile: code notifying: requestor
    "Compile the argument, code, and enter the result in the receiver's method dictionary. If an error occurs, send an
     appropriate message to the argument, requestor."
    self notImplementedYet.

- recompile: selector
    "Compile the method associated with the message selector, selector."
    self notImplementedYet.

- decompile: selector
    "Find the compiled code associated with the argument, selector, and decompile it. Answer the resulting source code
     as a String. If the selector is not in the method dictionary, report an error."
    self notImplementedYet.

- compileAll
    "Compile all the methods in the receiver's method dictionary."
    self notImplementedYet.

- compileAllSubclasses
    "Compile all the methods in the receiver's subclasses' method dictionaries."
    self notImplementedYet.

"category: instance creation"

- new
    "Answer a new initialized instance of the receiver."
    ^ self basicNew initialize.

- basicNew
    "Primitive. Answer an instance of the receiver (which is a class)."
    <primitive: 70>

- new: anInteger
    "Answer an instance of the receiver with anlnteger number of indexed variables. Report an error if the receiver is
     not indexable."
    self notImplementedYet.

- basicNew: anInteger
    "Same as basicNew, except this method should not be overridden in a subclass."
    self notImplementedYet.

"category: creating a class hierarchy"

- superclass: aClass
    "Set the superclass of the receiver to be the argument, aClass."
    self notImplementedYet.

- addSubclass: aClass
    "Make the argument, aClass, be a subclass of the receiver."
    self notImplementedYet.

- removeSubclass: aClass
    "Remove the argument, aClass, from the subclasses of the receiver."
    self notImplementedYet.

"category: accessing the method dictionary"

- selectors
    "Answer a Set of all the message selectors specified in the receiver's local method dictionary."
    self notImplementedYet.

- allSelectors
    "Answer a Set of all the message selectors that instances of the receiver can understand. This consists of all
     message selectors in the receiver's method dictionary and in the dictionaries of each of the receiver's
     superclasses."
    self notImplementedYet.

- compiledMethodAt: selector
    "Answer the compiled method associated with the argument, selector, a message selector in the receiver's local
     method dictionary. Report an error if the selector can not be found."
    self notImplementedYet.

- sourceCodeAt: selector
    "Answer a String that is the source code associated with the argument, selector, a message selector in the
     receiver's local method dictionary. Report an error if the selector can not be found."
    self notImplementedYet.

- sourceMethodAt: selector
    "Answer a Text for the source code associated with the argument, selector, a message selector in the receiver's
     local method dictionary. This Text provides boldface emphasis for the message pattern part of the method. Report
     an error if the selector can not be found."
    self notImplementedYet.

"category: accessing instances and variables"

- allInstances
    "Answer a Set of all direct instances of the receiver."
    self notImplementedYet.

- someInstance
    "Answer an existing instance of the receiver."
    self notImplementedYet.

- instanceCount
    "Answer the number of instances of the receiver that currently exist."
    self notImplementedYet.

- instVarNames
    "Answer an Array of the instance variable names specified in the receiver."
    self notImplementedYet.

- subclassInstVarNames
    "Answer a Set of the instance variable names specified in the receiver's subclasses."
    self notImplementedYet.

- allInstVarNames
    "Answer an Array of the names of the receiver's instance variables, those specified in the receiver and in all of
     its superclasses. The Array ordering is the order in •which the variables are stored and accessed by the
     Smalltalk-80 interpreter."
    self notImplementedYet.

- classVarNames
    "Answer a Set of the class variable names specified locally in the receiver."
    self notImplementedYet.

- allClassVarNames
    "Answer a Set of the names of the receiver's and the receiver's superclasses' class variables."
    self notImplementedYet.

- sharedPools
    "Answer a Set of the names of the pools (dictionaries) that are specified locally in the receiver."
    self notImplementedYet.

- allSharedPools
    "Answer a Set of the names of the pools (dictionaries) that are specified in the receiver and each of its
     superclasses."
    self notImplementedYet.

"category: accessing class hierarchy"

- subclasses
    "Answer a Set containing the receiver's immediate subclasses."
    self notImplementedYet.

- allSubclasses
    "Answer a Set of the receiver's subclasses and the receiver's descendent's subclasses."
    self notImplementedYet.

- withAllSubclasses
    "Answer a Set of the receiver, the receiver's subclasses and the receiver's descendent's subclasses."
    self notImplementedYet.

- superclass
    "Answer the receiver's immediate superclass."
    <primitive: 132>

- allSuperclasses
    "Answer an OrderedCollection of the receiver's superclass and the receiver's ancestor's superclasses.
     The first element is the receiver's immediate superclass, followed by its superclass, and so on; the last element
     is always Object."
    self notImplementedYet.

"category: testing the method dictionary"

- hasMethods
    "Answer whether the receiver has any methods in its (local) method dictionary."
    self notImplementedYet.

- includesSelector: aSymbol
    "Answer whether the message whose selector is the argument is in the method dictionary of the receiver's class."
    <primitive: 130>

- canUnderstand: selector
	"Answer whether the receiver can respond to the message whose selector is the argument. The selector can be in the
	 method dictionary of the receiver's class or any of its superclasses."
	<primitive: 139>

- whichClasslncludesSelector: selector
    "Answer the first class on the receiver's superclass chain where the argument, selector, can be found as a message
     selector. Answer nil if no class includes the selector."
    self notImplementedYet.

- whichSelectorsAccess: instVarName
    "Answer a Set of selectors from the receiver's local method dictionary whose methods access the argument,
     instVarName, as a named instance variable."
    self notImplementedYet.

- whichSelectorsReferTo: anObject
    "Answer a Set of selectors whose methods access the argument, anObject."
    self notImplementedYet.

- scopeHas: name ifTrue: aBlock
    "Determine whether the variable name, name, is within the scope of the receiver, i.e., it is specified as a variable
     in the receiver or in one of its superclasses. If so, evalaute the argument, aBlock."
    self notImplementedYet.

"category: adding/removing methods"

- atSelector: selector put: method
    <primitive: 129>

"category: testing the form of the instances"

- isPointers
    "Answer whether the variables of instances of the receiver are stored as pointers (words)."
    self notImplementedYet.

- isBits
    "Answer whether the variables of instances of the receiver are stored as bits (i.e., not pointers)."
    self notImplementedYet.

- isBytes
    "Answer whether the variables of instances of the receiver are stored as bytes (8-bit integers)."
    self notImplementedYet.

- isWords
    "Answer whether the variables of instances of the receiver are. stored as words."
    self notImplementedYet.

- isFixed
    "Answer true if instances of the receiver do not have indexed instance variables; answer false otherwise."
    self notImplementedYet.

- isVariable
    "Answer true if instances of the receiver do have indexed instance variables; answer false otherwise."
    self notImplementedYet.

- instSize
    "Answer the number of named instance variables of the receiver."
    self notImplementedYet.

"category: testing class hierarchy"

- inheritsFrom: aClass
    "Answer whether the argument, aClass, is on the receiver's superclass chain."
    | aSuperclass |
    aSuperclass := self superclass.
    [ aSuperclass == nil ]
      whileFalse: [
        aSuperclass == aClass ifTrue: [ ^ true ].
        aSuperclass := aSuperclass superclass.
      ].
	^ false.

- isKindOfSubclass
    "Answer a String that is the keyword that describes the receiver as a class: either a regular (fixed length)
     subclass, a variableSubctass, a variableByteSubclass, or a variableWordSubclass."
    self notImplementedYet.

"category: enumerating"

- allSubclassesDo: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's subclasses."
    self notImplementedYet.

- allSuperclassesDo: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's superclasses."
    self notImplementedYet.

- alllnstancesDo: aBlock
    "Evaluate the argument, aBlock, for each of the current instances of the receiver."
    self notImplementedYet.

- allSubinstancesDo: aBlock
    "Evaluate the argument, aBlock, for each of the current instances of the receiver's subclasses."
    self notImplementedYet.

- selectSubclasses: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's subclasses. Collect into a Set only those subclasses for
     which aBlock evaluates to true. Answer the resulting Set."
    self notImplementedYet.

- selectSuperclasses: aBlock
    "Evaluate the argument, aBIock, with each of the receiver's superclasses. Collect into a Set only those superclasses
     for which aBlock evaluates to true. Answer the resulting Set."
    self notImplementedYet.

- classVariableString
    "Answer a collection of the receiver's class variable names."
    self notImplementedYet.

- instanceVariableString
    ""
    self notImplementedYet.

- sharedVariableString
    ""
    self notImplementedYet.
