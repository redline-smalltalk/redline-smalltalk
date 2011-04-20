
Object subclass: #TestCase instanceVariableNames: 'testSelector'
        classVariableNames: 'history'
        poolDictionaries: ''
        category: 'SUnit-Kernel'.

	def yourself
		^ self

	def assert: aBoolean
		aBoolean ifFalse: [self signalFailure: 'Assertion failed']

	def assert: aBooleanOrBlock description: aString
		aBooleanOrBlock value ifFalse: [ self logFailure: aString.]

	def assert: aBooleanOrBlock description: aString resumable: resumableBoolean 
		| exception |
		aBooleanOrBlock value
			ifFalse: 
				[self logFailure: aString.
                        exception := resumableBoolean
                                                ifTrue: [TestResult resumableFailure]
                                                ifFalse: [TestResult failure].
                        exception signal: aString]

