Object subclass: #TestAsserter
	instanceVariableNames: ''
	classVariableNames: ''
	classInstanceVariableNames: ''
	poolDictionaries: ''.

+ assert: aBoolean description: aString
	"Minimal clone of the instance-side assert protocol so that class-side methods can use it."

	aBoolean ifFalse:
		[self logFailure: aString.
		TestResult failure signal: aString].

+ failureLog
	^Transcript

+ isLogging
	"By default, we're not logging failures. Override in subclasses as desired."

	^false

+ logFailure: aString
	self isLogging ifTrue:
		[self failureLog cr; nextPutAll: aString; flush].

- assert: aBoolean
	aBoolean ifFalse:
		[self logFailure: 'Assertion failed'.
		TestResult failure signal: 'Assertion failed'].

- assert: aBoolean description: aString
	aBoolean ifFalse:
		[self logFailure: aString.
		TestResult failure signal: aString].

- assert: aBoolean description: aString resumable: resumableBoolean
	| exception |
	aBoolean ifFalse:
		[self logFailure: aString.
		exception := resumableBoolean
			ifTrue: [TestResult resumableFailure]
			ifFalse: [TestResult failure].
		exception signal: aString].

- assert: anObject equals: anotherObject
	self
		assert: anObject = anotherObject
		description: anObject printString , ' is not equal to ' , anotherObject printString.

- deny: aBoolean
	self assert: aBoolean not.

- deny: aBoolean description: aString
	self assert: aBoolean not description: aString.

- deny: aBoolean description: aString resumable: resumableBoolean
	self assert: aBoolean not description: aString resumable: resumableBoolean.

- executeShould: aBlock inScopeOf: anExceptionalEvent
	^[aBlock value.
 	false]
		on: anExceptionalEvent
		do: [:ex | ex sunitExitWith: true]

- logFailure: aString
	self class logFailure: aString.

- should: aBlock raise: anExceptionalEvent 
	self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent).

- should: aBlock raise: anExceptionalEvent description: aString 
	self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString.

- shouldnt: aBlock raise: anExceptionalEvent 
	self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not.

- shouldnt: aBlock raise: anExceptionalEvent description: aString 
	self
		assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not
 		description: aString.

