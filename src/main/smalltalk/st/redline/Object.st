" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ProtoObject < #Object
- initialize
	"Subclasses should redefine this method to perform initializations on instance creation"

- == anObject 
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."

	<primitive: 110>

- at: index
    "Primitive.  Assuming the object is indexable, return the value at index."
    <primitive: 60>

- at: index put: value
    "Primitive.  Assuming the object is indexable, set the value at index."
    <primitive: 61>

"testing"
- class
	<primitive: 111>
	
- isKindOf: aClass
	self class == aClass
		ifTrue: [ ^ true ]
		ifFalse: [ ^ self class inheritsFrom: aClass ].
		
- isMemberOf: aClass
	^ self class == aClass.

- respondsTo: aSymbol
	^ self class canUnderstand: aSymbol.