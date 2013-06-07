"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ProtoObject subclass: #Object.

"category: initialization"

+ initialize
    "Subclasses should redefine this method to perform initializations on Class creation"

- initialize
    "Subclasses should redefine this method to perform initializations on instance creation"

"category: class membership"

- class
    "Primitive. Answer the object which is the receiver's class."
    <primitive: 111>

"category: evaluating"

- value
    ^ self.

"category: accessing"

- class
    "Primitive. Answer the object which is the receiver's class."
    <primitive: 111>

- yourself
    ^ self.

- at: index
    "Answer the value of the indexed instance variable of the receiver whose index is the argument, index. If the
     receiver does not have indexed variables, or if the argument is greater than the number of indexed variables, then
     report an error."
    <primitive: 144>

- at: index put: anObject
    "Store the argument, anObject, as the value of the indexed instance variable of the receiver whose index is the
     argument, index. If the receiver does not have indexed variables, or if the argument is greater than the number of
     indexed variables, then report an error. Answer anObject."
    <primitive: 143>

- size
    "Answer the receiver's number of indexed variables. This value is the same as the largest legal index."
    <primitive: 62>

- basicSize
    "Same as size. The method associated with this message, however, cannot be modified in any subclass."
    <primitive: 62>

- basicAt: index
    "Answer the value of the indexed instance variable of the receiver whose index is the argument, index. If the
     receiver does not have indexed variables, or if the argument is greater than the number of indexed variables, then
     report an error."
    <primitive: 144>

- basicAt: index put: anObject
    "Store the argument, anObject, as the value of the indexed instance variable of the receiver whose index is the
     argument, index. If the receiver does not have indexed variables, or if the argument is greater than the number of
     indexed variables, then report an error. Answer anObject."
    <primitive: 143>

"category: comparing"

- = anObject
    "Answer whether the receiver and the argument represent the same object."
    ^ self == anObject.

- == anObject
    "Primitive. Answer whether the receiver and the argument are the same object (have the same object pointer).
     Do not redefine the message == in any other class! Essential. No Lookup. Do not override in any subclass.
     NOTE: The Blue Book says just: Answer whether the receiver and the argument are the same object."
    <primitive: 110>

- ~= anObject
    "Answer whether the receiver and the argument do not represent the same object."
    ^ self = anObject == false.

- ~~ anObject
    "Answer whether the receiver and the argument are not the same object."
    ^ (self == anObject) not.

- identityHash
    "Answer a SmallInteger whose value is related to the receiver's identity. This method must not be overridden,
     except by SmallInteger. Do not override."
    <primitive: 75>

- hash
    "Answer an Integer computed with respect to the representation of the receiver."
    ^ self identityHash.

"category: class membership"

- isKindOf: aClass
    "Answer whether the argument, aClass, is a superclass or class of the receiver."
    ^ self class == aClass
        ifTrue: [ true ]
        ifFalse: [ self class inheritsFrom: aClass ].

- isMemberOf: aClass
    "Answer whether the receiver is an instance of the class, aClass."
    ^ self class == aClass.

- respondsTo: aSymbol
    "Answer whether the method dictionary of the receiver's class contains aSymbol as a message selector."
    ^ self class canUnderstand: aSymbol.

"category: copying"

- copy
    "Answer another instance just like the receiver. Subclasses typically override postCopy; they typically do not
     override shallowCopy. Copy is a template method in the sense of Design Patterns. So do not override it.
     Override postCopy instead. Pay attention that normally you should call postCopy of your superclass too."
    ^ self shallowCopy postCopy.

- postCopy
	"I'm a hook method in the sense of Design Patterns TemplateHook/Methods. I'm called by copy. self is a shallow copy,
	 subclasses should copy fields as necessary to complete the full copy"
	^ self.

- shallowCopy
    "Answer a copy of the receiver which shares the receiver's instance variables. It should never be overridden. I'm
     invoked from the copy template method. Subclasses that need to specialize the copy should specialize the postCopy
     hook method."
    self notImplementedYet.

- deepCopy
    "Answer a copy of the receiver with its own copy of each instance variable. deepCopy does a deep copy.
     It should never be overridden and only be used if you want to get these very specific semantics."
    self notImplementedYet.

"category: dependents access"

- addDependent: anObject
    "Make the given object one of the receiver's dependents."
    self notImplementedYet.

- removeDependent: anObject
    "Remove the given object as one of the receiver's dependents."
    self notImplementedYet.

- dependents
    "Answer a collection of objects that are 'dependent' on the receiver; that is, all objects that should be notified
     if the receiver changes."
    self notImplementedYet.

- release
    "Remove references to objects that may refer to the receiver. This message should be overridden by subclasses with
     any cycles, in which case the subclass should also include the expression super release."
    self notImplementedYet.

- broadcast: aSymbol
    self notImplementedYet.

- broadcast: aSymbol with: anObject
    self notImplementedYet.

"category: updating"

- changed
    "Receiver changed in a general way; inform all the dependents by sending each dependent an update: message."
	self changed: self.

- changed: aParameter
    "Receiver changed. The change is denoted by the argument aParameter. Usually the argument is a Symbol that is part
     of the dependent's change protocol. Inform all of the dependents."
    self notImplementedYet.

- update: aParameter
    "Receive a change notice from an object of whom the receiver is a dependent. The default behavior is to do nothing;
     a subclass might want to change itself in some way."
	^ self.

"category: error handling"

- error: aString
    "Report to the user that an error occurred in the context of responding to a message to the receiver. The report
     uses the argument, aString, as part of the error notification comment."
    <primitive: 131>

- doesNotUnderstand: aMessage
    "Report to the user that the receiver does not understand the argument, aMessage, as a message."
    self error: aMessage.

- primitiveFailed
    "Announce that a primitive has failed and there is no appropriate Smalltalk code to run."
    self error: 'a primitive has failed'.

- shouldNotImplement
    "Announce that, although the receiver inherits this message, it should not implement it."
    self error: 'This message is not appropriate for this object'.

- subclassResponsibility
    "This message sets up a framework for the behavior of the class' subclasses. Announce that the subclass should have
     implemented this message."
    self error: 'My subclass should have overridden one of my messages.'.

- notImplementedYet
    "Announce that, although the receiver inherits this message, it is not implemented YET."
    self error: 'This message is not implemented yet. Please implement is via a Test.'.

"category: testing"

- isNil
    "Coerces nil to true and everything else to false."
    ^ false.

- notNil
    "Coerces nil to false and everything else to true."
    ^ true.

- ifNil: aBlock
    "Return self, or evaluate the block if I'm == nil - NOTE: This is not actually in the Blue Book."
    ^ self.

"category: message handling"

- perform: aSymbol
    "Send the unary selector, aSymbol, to the receiver."
    <primitive: 83>

- perform: aSymbol with: anObject
    "Send the selector, aSymbol, to the receiver with anObject as its argument."
    <primitive: 84>

- perform: aSymbol with: firstObject with: secondObject
    "Send the selector, aSymbol, to the receiver with the given arguments."
    <primitive: 140>

- perform: aSymbol with: firstObject with: secondObject with: thirdObject
    "Send the selector, aSymbol, to the receiver with the given arguments."
    <primitive: 141>

- perform: aSymbol withArguments: arguments
    "Send the selector, aSymbol, to the receiver with the given arguments."
    <primitive: 142>

"category: printing"

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

"category: storing"

- storeString
    "Answer a String representation of the receiver from which the receiver can be reconstructed."
    ^ String streamContents: [:s | self storeOn: s].

"category: system primitives"

- instVarAt: index
    "Primitive. Answer a fixed variable in an object. The numbering of the variables corresponds to the named instance
     variables. Fail if the index is not an Integer or is not the index of a fixed variable."
    <primitive: 73>

- instVarAt: anInteger put: anObject
    "Primitive. Store a value into a fixed variable in the receiver. The numbering of the variables corresponds to the
     named instance variables. Fail if the index is not an Integer or is not the index of a fixed variable. Answer the
     value stored as the result. Using this message violates the principle that each object has sovereign control over
     the storing of values into its instance variables."
    <primitive: 74>

- become: otherObject
    self error: 'implement me'.
