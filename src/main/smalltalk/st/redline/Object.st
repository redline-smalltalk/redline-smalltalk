" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ProtoObject < #Object

+ initialize
  "Subclasses should redefine this method to perform initializations on Class creation"

- initialize
  "Subclasses should redefine this method to perform initializations on instance creation"

"comparing"
- == anObject 
  "Primitive. Answer whether the receiver and the argument are the same
  object (have the same object pointer). Do not redefine the message == in
  any other class! Essential. No Lookup. Do not override in any subclass.
  See Object documentation whatIsAPrimitive."
  
  "NOTE: The Blue Book says just:
  Answer whether the receiver and the argument are the same object."
  <primitive: 110>

- = anObject
  "Answer whether the receiver and the argument represent the same component."
  ^ self == anObject.

- ~= anObject
  "Answer whether the receiver and the argument do not represent the same component."
  ^ self = anObject == false.

- ~~ anObject
  "Answer whether the receiver and the argument are not the same object."
  ^ self ~= anObject.

- hash
  "Answer an Integer computed with respect to the representation of the receiver."
  <primitive: 75>

"testing"

- ifNil: nilBlock
  "Return self, or evaluate the block if I'm == nil"
  "NOTE: This is not actually in the Blue Book."
  ^ self.

- isNil
  "Answer whether the receiver is nil."
  ^ false.

- notNil
  "Answer whether the receiver is not nil."
  ^ true.

- class
  "Answer the object which is the receiver's class."
  <primitive: 111>

- isKindOf: aClass
  "Answer whether the argument, aClass, is a superclass or class of the receiver."
  ^ self class == aClass
    ifTrue: [ true ]
    ifFalse: [ self class inheritsFrom: aClass ].

- isMemberOf: aClass
  "Answer whether the receiver is a direct instance
  of the argument, aClass. This is the same as testing whether the response to sending
  the receiver the message class is the same as (= =) aClass."
  ^ self class == aClass.

- respondsTo: aSymbol
  "Answer whether the method dictionary of the receiver's class or one of its superclasses contains the argument, aSymbol, as a message selector."
  ^ self class canUnderstand: aSymbol.

"accessing"

- at: index
  "Answer the value of the indexed instance variable of the receiver whose
  index is the argument, index. If the receiver does not have indexed variables,
  or if the argument is greater than the number of indexed variables, then
  report an error."
  ^ self basicAt: index.

- at: index put: anObject
  "Store the argument, anObject, as the value of the indexed instance variable of
  the receiver whose index is the argument, index. If the receiver does not have
  indexed variables, or if the argument is greater than the number of indexed
  variables, then report an error. Answer anObject."
  ^ self basicAt: index put: value.

- basicAt: index
  "Primitive.  Assuming the object is indexable, return the value at index."
  <primitive: 60>

- basicAt: index put: value
  "Primitive.  Assuming the object is indexable, set the value at index."
  <primitive: 61>

- size
  "Answer the receiver's number of indexed variables. This value is the same
  as the largest legal index."
  ^ self basicSize.

- basicSize
  "Same as size. The method associated with this message, however, cannot be
  modified in any subclass."
  <primitive: 62>

- value
  ^ self.

+ error: aMessage
  <primitive: 216>

+ testClassMethod
  Transcript show: 'test'; cr.
