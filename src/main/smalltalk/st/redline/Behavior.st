" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Behavior
	instanceVariableNames: 'superclass'.

- new
	"Answer a new initialized instance of the receiver."
	^ self basicNew initialize.

- basicNew
	"Primitive. Answer an instance of the receiver (which is a class)." 
	<primitive: 70>

- superclass
	<primitive: 219>

- inheritsFrom: aClass
"	| aSuperclass |
	aSuperclass := superclass.
	[ aSuperclass == nil ]
		whileFalse:
			[ aSuperclass == aClass ifTrue: [ ^ true ].
			aSuperclass := aSuperclass superclass].
	^ false
"

- canUnderstand: selector
  "Answer whether the receiver can respond to the message whose selector
  is the argument. The selector can be in the method dictionary of the
  receiver's class or any of its superclasses."
  <primitive: 220>

- includesSelector: aSymbol
  "Answer whether the message whose selector is the argument is in the
  method dictionary of the receiver's class."
  <primitive: 221>
