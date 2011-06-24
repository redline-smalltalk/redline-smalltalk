TestAsserter subclass: #TestCase
	instanceVariableNames: 'testSelector'
	classVariableNames: 'Announcers'
	classInstanceVariableNames: 'history'
	poolDictionaries: ''.

+ addTestsFor: classNameString toSuite: suite
	| cls |
	cls := Smalltalk globals at: classNameString ifAbsent: [ ^ suite ].
	^ cls isAbstract
		ifTrue: [ 
			cls allSubclasses
				do: [ :each | 
					each isAbstract
						ifFalse: [ each addToSuiteFromSelectors: suite ] ].
			suite ]
		ifFalse: [ cls addToSuiteFromSelectors: suite ]

+ addToSuite: suite fromMethods: testMethods 
	testMethods do:  [ :selector | 
			suite addTest: (self selector: selector) ].
	^suite

+ addToSuiteFromSelectors: suite
	^self addToSuite: suite fromMethods: (self shouldInheritSelectors
		ifTrue: [ self allTestSelectors ]
		ifFalse: [self testSelectors ])

+ allTestMethods
	^ self  methods select: [:aCompiledMethod| 
							   aCompiledMethod isTestMethod].

+ allTestSelectors
	| answer pivotClass lookupRoot |
	answer := Set withAll: self testSelectors.
	self shouldInheritSelectors
		ifTrue:
			[pivotClass := self.
			lookupRoot := self lookupHierarchyRoot.
			[pivotClass == lookupRoot]
				whileFalse:
					[pivotClass := pivotClass superclass.
					answer addAll: pivotClass testSelectors]].
	^answer asSortedCollection asOrderedCollection

+ announcer 
	^ self announcers at: self ifAbsentPut: [ Announcer new ]

+ announcers
	^ Announcers ifNil: [ Announcers := Dictionary new ]

+ browserIcon
	| classHistory |
	self isAbstract
		ifTrue: [ ^ super browserIcon ].
	classHistory := TestResult historyFor: self.
	(classHistory at: #errors) isEmpty
		ifFalse: [ ^ #testRed ].
	(classHistory at: #failures) isEmpty
		ifFalse: [ ^ #testOrange ].
	(classHistory at: #passed) isEmpty
		ifFalse: [ ^ #testGreen ].
	^ #testGray

+ browserIcon: aClassDescription selector: aSelector
	(aClassDescription isMeta or: [ aClassDescription isAbstract or: [ (aSelector beginsWith: 'test') not ] ])
		ifTrue: [ ^ super browserIcon: aClassDescription selector: aSelector ].
	(aClassDescription methodRaisedError: aSelector)
		ifTrue: [ ^ #testRed ].
	(aClassDescription methodFailed: aSelector)
		ifTrue: [ ^ #testOrange ].
	(aClassDescription methodPassed: aSelector)
		ifTrue: [ ^ #testGreen ].
	^ #testGray

+ buildSuite
	| suite |
	^self isAbstract
		ifTrue: 
			[suite := self suiteClass named: self name asString.
			self allSubclasses 
				do: [:each | each isAbstract ifFalse: [suite addTest: each buildSuiteFromSelectors]].
			suite]
		ifFalse: [self buildSuiteFromSelectors]

+ buildSuiteFromAllSelectors

	^self buildSuiteFromMethods: self allTestSelectors

+ buildSuiteFromLocalSelectors

	^self buildSuiteFromMethods: self testSelectors

+ buildSuiteFromMethods: testMethods

	^testMethods
		inject: (self suiteClass named: self name asString)
		into: [:suite :selector |
			suite
				addTest: (self selector: selector);
				yourself]

+ buildSuiteFromSelectors
	^self buildSuiteFromMethods: self allTestSelectors

+ coverage
	"returns the coverage determined by a simple static analysis of test coverage 
	made by the receiver on a class that is identified by the name of the receiver.
	We assume that SetTest test Set."

	| cls className |
	(self name endsWith: 'Test')
		ifFalse: [ self error: 'Please, use #coverageForClass: instead' ].
	className := self name copyFrom: 1 to: self name size - 'Test' size.
	cls := Smalltalk globals at: className asSymbol ifAbsent: [ self error: 'Please, use #coverageForClass: instead' ].	"May happen with Transcript"
	cls isBehavior
		ifFalse: [ cls := cls class ].
	^ self coverageForClass: cls

+ coverageAsString
	| cov className |
	cov := self coverage first asInteger. 
	"coverage already checks that the name is ends with 'Test' and if the class tested exists"
	
	className := self name copyFrom: 1 to: (self name size - 'Test' size).
	^ self name asString, ' covers ', cov asString, '% of ', className.

+ coverageForClass: cls
	"returns the test coverage of all the methods included inherited ones"
	^ self coverageForClass: cls until: ProtoObject

+ coverageForClass: cls until: aRootClass
	"returns the test coverage of all the methods included inherited ones but stopping at aRootClass included"
	
	| definedMethods testedMethods untestedMethods |
	definedMethods := cls allSelectorsAboveUntil: aRootClass.
	definedMethods size = 0
		ifTrue: [^ {0. Set new}].
	testedMethods := 
		self methodDictionary values inject: Set new into: 
							[:sums :cm | sums union: cm messages].
	testedMethods := testedMethods reject: [:sel | (definedMethods includes: sel) not].
	untestedMethods := definedMethods select: [:selector | (testedMethods includes: selector) not].
	^ { (testedMethods size * 100 / definedMethods size) asFloat . untestedMethods}

+ coveragePercentage
	^ self coverage first

+ debug: aSymbol

	^(self selector: aSymbol) debug

+ generateLastStoredRunMethod

	self shouldGenerateLastStoredRunMethod ifTrue: [
		self class
			compile: (self lastRunMethodNamed: #lastStoredRun)
			classified: 'history' ]

+ hasMethodBeenRun: aSelector
	^ ((self lastRun at: #errors),
		(self lastRun at: #failures),
		(self lastRun at: #passed))
			includes: aSelector

+ history
	^ history ifNil: [ history := self newTestDictionary ]

+ history: aDictionary
	history := aDictionary

+ initialize
     super initialize.
	SystemChangeNotifier uniqueInstance notify: self ofSystemChangesOfItem: #method using: #methodChanged:.

+ isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestCase

+ isUnitTest
	^true

+ lastRun
	^ TestResult historyFor: self

+ lastRunMethodNamed: aSelector
	
	^ String streamContents: [:str |
		str nextPutAll: aSelector asString ;cr.
		str tab; nextPutAll: '^ ', (self lastRun) storeString]

+ lastStoredRun
	^ ((Dictionary new) add: (#failures->#()); add: (#passed->#()); add: (#errors->#()); yourself)

+ localCoverage
	"returns the coverage determined by a simple static analysis of test coverage 
	made by the receiver on a class that is identified by the name of the receiver.
	We assume that SetTest test Set. The computation of the coverage takes only into
	account the methods defined locally in the tested class. See coverage for a more global 
	coverage"

	| cls className |
	(self name endsWith: 'Test')
		ifFalse: [ self error: 'Please, use #localCoverageForClass: instead' ].
	className := self name copyFrom: 1 to: self name size - 'Test' size.
	cls := Smalltalk globals at: className asSymbol ifAbsent: [ self error: 'Please, use #localCoverageForClass: instead' ].
	cls isBehavior
		ifFalse: [ cls := cls class ].
	^ self localCoverageForClass: cls

+ localCoverageAsString
	| cov className |
	cov := self localCoverage first asInteger. 
	"coverage already checks that the name is ends with 'Test' and if the class tested exists"
	
	className := self name copyFrom: 1 to: (self name size - 'Test' size).
	^ self name asString, ' covers ', cov asString, '% of ', className.

+ localCoverageForClass: cls
	
	| definedMethods testedMethods untestedMethods |
	definedMethods := cls selectors.
	"It happens for IdentityBag / IdentityBagTest"
	definedMethods size = 0
		ifTrue: [^ {0. Set new}].

	testedMethods := 
		self methodDictionary values inject: Set new into: 
							[:sums :cm | sums union: cm messages].
					
	"testedMethods contains all the methods send in test methods, which probably contains methods that have nothign to do with collection"
	testedMethods := testedMethods reject: [:sel | (definedMethods includes: sel) not].

	untestedMethods := definedMethods select: [:selector | (testedMethods includes: selector) not].

	^ { (testedMethods size * 100 / definedMethods size) asFloat . untestedMethods}

+ localCoveragePercentage
	^ self localCoverage first

+ lookupHierarchyRoot
	^TestCase

+ methodChanged: anEvent
	"Remove the changed method from the known test results."
	
	| cls sel |
	anEvent item isCompiledMethod ifFalse: [ ^ self ].
	cls := anEvent item methodClass.
	(cls inheritsFrom: TestCase)
		ifFalse: [^ self].
	sel := anEvent item selector.
	(sel beginsWith: 'test')
		ifFalse: [^ self].
	TestResult removeFromTestHistory: sel in: cls.

+ methodFailed: aSelector
	^ (self lastRun at: #failures) includes: aSelector

+ methodPassed: aSelector
	^ (self lastRun at: #passed) includes: aSelector

+ methodProgressed: aSelector
	^ ((self storedMethodRaisedError: aSelector) or: [self storedMethodFailed: aSelector])
		and: [self methodPassed: aSelector]

+ methodRaisedError: aSelector
	^ (self lastRun at: #errors) includes: aSelector

+ methodRegressed: aSelector
	^ (self storedMethodPassed: aSelector) and: [(self methodFailed: aSelector) or: [self methodRaisedError: aSelector]]

+ newTestDictionary

	^ Dictionary new at: #timeStamp put: DateAndTime now;
		at: #passed put: Set new;
		at: #failures put: Set new;
		at: #errors put: Set new;
		yourself

+ packageNamesUnderTest
	"Answer a collection of package names under test. This is used by the test runner to automatically instrument the code in these packages when checking for test coverage."
	
	^ #()

+ resetAnnouncer
	self announcers removeKey: self ifAbsent: []

+ resetHistory
	history := nil

+ resources

	^#()

+ run: aSymbol

	^(self selector: aSymbol) run

+ selector: aSymbol

	^self new setTestSelector: aSymbol

+ shouldAnnounce
	^ self announcers includesKey: self

+ shouldGenerateLastStoredRunMethod
	| sameRun |
	
	(self class methodDictionary includesKey: #lastStoredRun)
		ifFalse: [^ true].
	sameRun := #(#passed #failures #errors) inject: true into: 
		[ :ok :set | ok and: [(self lastRun at: set) = (self lastStoredRun at: set) ]].
	^ sameRun not

+ shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."
	
	^self ~~ self lookupHierarchyRoot
		and: [self superclass isAbstract or: [self testSelectors isEmpty]]

+ storedMethodFailed: aSelector
	^ (self lastStoredRun at: #failures) includes: aSelector

+ storedMethodPassed: aSelector
	^ (self lastStoredRun at: #passed) includes: aSelector

+ storedMethodRaisedError: aSelector
	^ (self lastStoredRun at: #errors) includes: aSelector

+ suite

	^self buildSuite

+ suiteClass
	^TestSuite

+ sunitVersion
	^'4.0'

+ testSelectors
	^(self selectors select: [ :each | (each beginsWith: 'test') and: [each numArgs isZero]])

- addDependentToHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"

- announce: anAnnouncementClass
	^ self announce: anAnnouncementClass with: nil

- announce: anAnnouncementClass withResult: result
	| event |
	self shouldAnnounce ifFalse: [ ^ self ].
	
	event := anAnnouncementClass asAnnouncement.
	event testCase: self.
	event testSelector: testSelector.
	event result: result.
	self announcer announce: event.

- announcer
	^ self class announcer

- assert: aBoolean

	aBoolean ifFalse: [self signalFailure: 'Assertion failed']

- assert: aBooleanOrBlock description: aString
	aBooleanOrBlock value ifFalse: [
		self logFailure: aString.
		TestResult failure signal: aString]

- assert: aBooleanOrBlock description: aString resumable: resumableBoolean 
	| exception |
	aBooleanOrBlock value
		ifFalse: 
			[self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception signal: aString]

- assert: expected equals: actual
	^ self
		assert: (expected = actual)
		description: (self comparingStringBetween: expected and: actual)

- cleanUpInstanceVariables
	self class allInstVarNames do: [ :name |
		name = 'testSelector' ifFalse: [
			self instVarNamed: name put: nil ] ]

- comparingStringBetween: expected and: actual
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Expected ';
			nextPutAll: (expected printStringLimitedTo: 10);
			nextPutAll: ' but was ';
			nextPutAll: (actual printStringLimitedTo: 10);
			nextPutAll: '.'
		]

- debug
	[(self class selector: testSelector) runCase]
		ensure: [TestResource resetResources: self resources]

- debugAsFailure
	| semaphore |
	semaphore := Semaphore new.
	[semaphore wait.
	TestResource resetResources: self resources] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore

- deny: aBoolean

	self assert: aBoolean not

- deny: aBooleanOrBlock description: aString
	self assert: aBooleanOrBlock value not description: aString

- deny: aBooleanOrBlock description: aString resumable: resumableBoolean 
	self
		assert: aBooleanOrBlock value not
		description: aString
		resumable: resumableBoolean

- executeShould: aBlock inScopeOf: anExceptionalEvent 
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: true]

- executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: aString
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubString: aString) ]

- executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: aString
	^[aBlock value.
 	false] on: anExceptionalEvent
		do: [:ex | ex return: (ex description includesSubString: aString) not ]

- executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock

	^[aBlock value.
 	false] 
		on: anException
		do: [:exception | 
			anotherBlock value: exception.
			exception return: true]

- expectedFailures
	"| pragmas |
	pragmas := Pragma allNamed: #expectedFailure from: self class to: Object.
	^ pragmas collect: [:each | each method selector  ].
	
	this implementation is far too slow and block the execution of tests since just for printing a test
	it scans all the literal of the system.
	So I reverted the old way of declaring expectedFailures
	"
	
	^ Array new

- fail

	^self assert: false

- failureLog	
	^Transcript

- isLogging
	"By default, we're not logging failures. If you override this in 
	a subclass, make sure that you override #failureLog"
	^false

- logFailure: aString
	self isLogging ifTrue: [
		self failureLog 
			cr; 
			nextPutAll: aString; 
			flush]

- openDebuggerOnFailingTestMethod
	"SUnit has halted one step in front of the failing test method. Step over the 'self halt' and 
	 send into 'self perform: testSelector' to see the failure from the beginning"

	self
		halt;
		performTest

- performTest
	self perform: testSelector asSymbol

- prepareToRunAgain 	

	"Used by the debugger when a test is restarted
	I'm not 'hidding'' possible exceptions because I think it is best to popup a new debugger
	if some one occurs and let the programmer decide what to do - Hernan"
	self 
		tearDown;
		setUp

- printOn: aStream

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: testSelector

- removeDependentFromHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"

- resources
	"We give TestCase this instance-side method so that methods polymorphic with TestSuite can be code-identical.  Having this instance-side method also helps when writing tests of resource behaviour. Except for such tests, it is rare to override this method and should not be done without thought.  If there were a good reason why a single test case needed to share tests requiring different resources, it might be legitimate."

	^self class resources

- run
	| result |
	result := TestResult new.
	[self run: result]
		ensure: [TestResource resetResources: self resources].
	^result

- run: aResult
	aResult runCase: self

- runCase
	self resources do: [:each | each availableFor: self].
	[self setUp.
	self performTest] ensure: [
		self tearDown.
		self cleanUpInstanceVariables]

- runCaseAsFailure: aSemaphore
	[self resources do: [:each | each availableFor: self].
	[self setUp.
	self openDebuggerOnFailingTestMethod] ensure: [self tearDown]]
		ensure: [aSemaphore signal]

- selector
	^testSelector

- setTestSelector: aSymbol
	testSelector := aSymbol

- setUp
	"intentionally empty method"

- should: aBlock
	self assert: aBlock value

- should: aBlock description: aString
	self assert: aBlock value description: aString

- should: aBlock notTakeMoreThan: aDuration
    "Evaluate aBlock in a forked process and if it takes more than anInteger milliseconds
    to run we terminate the process and report a test failure.  It'' important to
    use the active process for the test failure so that the failure reporting works correctly
    in the context of the exception handlers."

    | evaluated evaluationProcess result delay testProcess |

    evaluated := false.
    delay := Delay forDuration: aDuration.
    testProcess := Processor activeProcess.
    "Create a new process to evaluate aBlock"
    evaluationProcess := [
        result := aBlock value.
        evaluated := true.
        delay unschedule.
        testProcess resume ] forkNamed: 'Process to evaluate should: notTakeMoreThanMilliseconds:'.

    "Wait the milliseconds they asked me to"
    delay wait.
    "After this point either aBlock was evaluated or not..."
    evaluated ifFalse: [
        evaluationProcess terminate.
        self assert: false description: ('Block evaluation took more than the expected <1p>' expandMacrosWith: aDuration)].
   
    ^result

- should: aBlock notTakeMoreThanMilliseconds: anInteger
    "For compatibility with other Smalltalks"

   self should: aBlock notTakeMoreThan: (Duration milliSeconds: anInteger).

- should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)

- should: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString

- should: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: subString description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: subString)
		description: aString

- should: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: subString description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: subString)
		description: aString

- should: aBlock raise: anException withExceptionDo: anotherBlock 

	^self assert: (self executeShould: aBlock inScopeOf: anException withExceptionDo: anotherBlock)

- shouldAnnounce
	^ self class shouldAnnounce

- shouldFix: aBlock 

	^self should: aBlock raise: Exception

- shouldPass
	"Unless the selector is in the list we get from #expectedFailures, we expect it to pass"
	^ (self expectedFailures includes: testSelector) not

- shouldnt: aBlock
	self deny: aBlock value

- shouldnt: aBlock description: aString
	self deny: aBlock value description: aString

- shouldnt: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not

- shouldnt: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not 		description: aString

- shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionDoesNotInclude: subString description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionNotContaining: subString) not
		description: aString

- shouldnt: aBlock raise: anExceptionalEvent whoseDescriptionIncludes: subString description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent withDescriptionContaining: subString) not
		description: aString

- signalFailure: aString
	TestResult failure signal: aString

- tearDown
	"intentionally empty method"

