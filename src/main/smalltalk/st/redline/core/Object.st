" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ProtoObject < #Object.

+ initialize
    "Subclasses should redefine this method to perform initializations on Class creation"

- initialize
    "Subclasses should redefine this method to perform initializations on instance creation"

"evaluating"

- value
    ^ self.

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

- isInteger
    "Overridden to return true in Integer."
    ^ false.

"accessing"

- at: index
    "Answer the value of the indexed instance variable of the receiver whose
    index is the argument, index. If the receiver does not have indexed variables,
    or if the argument is greater than the number of indexed variables, then
    report an error."
    <primitive: 136>

- at: index put: anObject
    "Store the argument, anObject, as the value of the indexed instance variable of
    the receiver whose index is the argument, index. If the receiver does not have
    indexed variables, or if the argument is greater than the number of indexed
    variables, then report an error. Answer anObject."
    <primitive: 137>

- basicAt: index
    "Same as at: index. The method associated with this message, however, cannot be modified in any subclass."
    <primitive: 60>

- basicAt: index put: anObject
    "Same as at: index put: anObject. The method associated with this message, however, cannot be modified in any subclass."
    <primitive: 61>

- size
    "Answer the receiver's number of indexed variables. This value is the same
    as the largest legal index."
    <primitive: 138>

- basicSize
    "Same as size. The method associated with this message, however, cannot be
    modified in any subclass."
    <primitive: 62>

"printing"

- printString
    "Answer a String whose characters are a description of the receiver."
    | stream |
    stream := WriteStream on: String new.
    self printOn: stream.
    ^ stream contents.

- printOn: aStream 
    "Append to the argument, aStream, a sequence of characters that identifies the receiver."
    | title |
    title := self class name.
    aStream
      nextPutAll: (title first isVowel ifTrue: [ 'an ' ] ifFalse: [ 'a ' ]);
      nextPutAll: title.

"storing"

- storeString
    "Answer a String representation of the receiver
    from which the receiver can be reconstructed."
  ^ String streamContents: [:s | self storeOn: s].

- yourself
    "Answer self."
    ^ self.

"error handling"

- error: aString
    "Report to the user that an error occurred in
    the context of responding to a message to the
    receiver. The report uses the argument,
    aString, as part of the error notification comment."
    <primitive: 216>

- doesNotUnderstand: aMessage 
    "Report to the user that the receiver does not understand the argument, aMessage, as a message."
    self error: aMessage

- primitiveFailed
    "Announce that a primitive has failed and there is no appropriate
    Smalltalk code to run."

    self error: 'a primitive has failed'

- shouldNotImplement
    "Announce that, although the receiver inherits this message, it should
    not implement it."

    self error: 'This message is not appropriate for this object'

- subclassResponsibility
    "This message sets up a framework for the behavior of the class'
    subclasses. Announce that the subclass should have implemented this
    message."

    self error: 'My subclass should have overridden one of my messages.'
