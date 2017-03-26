"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Behavior
	     instanceVariableNames: 'superclass methodDict format subclasses '
	     classVariableNames: ''
	     poolDictionaries: ''
	     category: 'Kernel-Classes'.

- addSelector: selector withMethod: compiledMethod
	self basicAddSelector: selector withMethod: compiledMethod.

"category: instance creation"

- new
    "Answer a new initialized instance of the receiver."
    ^ self basicNew initialize.

- basicNew
    "Primitive. Answer an instance of the receiver (which is a class)."
    JVM aload: 1;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveNew' matching: '()Lst/redline/core/PrimObject;'.
