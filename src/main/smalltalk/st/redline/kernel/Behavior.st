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

"category: creating a class hierarchy"

"category: accessing the method dictionary"

"category: accessing instances and variables"

"category: accessing class hierarchy"

- superclass
    <primitive: 132>

"category: adding/removing methods"

- atSelector: selector put: method
    <primitive: 129>

"category: testing the form of the instances"

"category: enumerating"

"category: testing the method dictionary"

- canUnderstand: selector
	"Answer whether the receiver can respond to the message whose selector is the argument. The selector can be in the
	 method dictionary of the receiver's class or any of its superclasses."
	<primitive: 139>

- includesSelector: aSymbol
    "Answer whether the message whose selector is the argument is in the method dictionary of the receiver's class."
    <primitive: 130>

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
