"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Behavior.

"category: instance creation"

- new
    "Answer a new initialized instance of the receiver."
    ^ self basicNew initialize.

- basicNew
    "Primitive. Answer an instance of the receiver (which is a class)."
    <primitive: 70>

"category: accessing"

- superclass
    <primitive: 132>

"category: adding/removing methods"

- atSelector: selector put: method
    <primitive: 129>

"category: testing method dictionary"

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
