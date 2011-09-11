" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Behavior

- new
	"Answer a new initialized instance of the receiver."
	^ self basicNew initialize

- basicNew
	"Primitive. Answer an instance of the receiver (which is a class)." 
	<primitive: 70>
