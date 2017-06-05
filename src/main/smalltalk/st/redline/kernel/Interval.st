"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a finite arithmetic progression.'"

SequenceableCollection subclass: #Interval
	                   instanceVariableNames: 'start stop step '
	                   category: 'Collections-Sequenceable'.

"Interval methodsFor: 'accessing'"

- at: anInteger
	"Answer the anInteger'th element."

	(anInteger >= 1 and: [anInteger <= self size])
		ifTrue: [^start + (step * (anInteger - 1))]
		ifFalse: [self errorSubscriptBounds: anInteger].

- at: anInteger put: anObject
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'.

- atPin: anInteger
	"Answer the anInteger'th element.  Return first or last if out of bounds."
    anInteger >= 1
        ifTrue: [anInteger <= self size
            ifTrue: [^start + (step * (anInteger - 1))]
            ifFalse: [^ self last]]
        ifFalse: [^ self first].

- atWrap: anInteger
	"Answer the anInteger'th element.  If index is out of bounds, let it wrap around from the end to the beginning until it is in bounds."

    ^ self at: (anInteger - self increment \\ self size + self increment).

- first
	"Refer to the comment in SequenceableCollection|first."

	^start.

- includes: aNumber
	^ aNumber between: self first and: self last.

- increment
	"Answer the receiver's interval increment."

	^step.

- last
	"Refer to the comment in SequenceableCollection|last."

	^stop - (stop - start \\ step).

- size

	step < 0
		ifTrue: [start < stop
				ifTrue: [^0]
				ifFalse: [^stop - start // step + 1]]
		ifFalse: [stop < start
				ifTrue: [^0]
				ifFalse: [^stop - start // step + 1]].


"Interval methodsFor: 'comparing'"

- = anInterval
        "Answer true if my species and anInterval species are equal, and
        if our starts, steps and sizes are equal."

        self species == anInterval species
            ifTrue: [^start = anInterval first
                        and: [step = anInterval increment and: [self size = anInterval size]]]
            ifFalse: [^false].

- hash
	"Hash is reimplemented because = is implemented."

	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: self size.

- hashMappedBy: map
	"My hash is independent of my oop."

	^self hash.


"Interval methodsFor: 'adding'"

- add: newObject
	"Adding to an Interval is not allowed."

	self shouldNotImplement.


"Interval methodsFor: 'removing'"

- remove: newObject
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'.


"Interval methodsFor: 'copying'"

- copy
	"Return a copy of me. Override the superclass because my species is
	Array and copy, as inherited from SequenceableCollection, uses
	copyFrom:to:, which creates a new object of my species."

	^self shallowCopy.


"Interval methodsFor: 'enumerating'"

- collect: aBlock
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do:
		[:i |
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step].
	^ result.

- do: aBlock

	| aValue |
	aValue := start.
	step < 0
		ifTrue: [[stop <= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue := aValue + step]]
		ifFalse: [[stop >= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue := aValue + step]].

- reverseDo: aBlock
	"Evaluate aBlock for each element of my interval, in reverse order."

	| aValue |
	aValue := stop.
	step < 0
		ifTrue: [[start >= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue := aValue - step]]
		ifFalse: [[start <= aValue]
				whileTrue:
					[aBlock value: aValue.
					aValue := aValue - step]].


"Interval methodsFor: 'printing'"

- printOn: aStream

	aStream nextPut: $(.
	start printOn: aStream.
	aStream nextPutAll: ' to: '.
	stop printOn: aStream.
	step ~= 1
		ifTrue:
			[aStream nextPutAll: ' by: '.
			step printOn: aStream].
	aStream nextPut: $).

- storeOn: aStream
	"This is possible because we know numbers store and print the same."

	self printOn: aStream.


"Interval methodsFor: 'private'"

- setFrom: startInteger to: stopInteger by: stepInteger

	start := startInteger.
	stop := stopInteger.
	step := stepInteger.

- species

	^Array.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Interval class methodsFor: 'instance creation'"

+ from: startInteger to: stopInteger
	"Answer an instance of me, starting at startNumber, ending at
	stopNumber, and with an interval increment of 1."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: 1.

+ from: startInteger to: stopInteger by: stepInteger
	"Answer an instance of me, starting at startNumber, ending at
	stopNumber, and with an interval increment of stepNumber."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: stepInteger.

+ new
	"Primitive. Create and answer with a new instance of the receiver
	(a class) with no indexable fields. Fail if the class is indexable. Override
	SequenceableCollection new. Essential. See Object documentation
	whatIsAPrimitive."

	JVM primitive: 70.

+ newFrom: aCollection
	"Answer an instance of me containing the same elements as aCollection."

	| newInterval |
	newInterval := self from: aCollection first to: aCollection last by:
		(aCollection last - aCollection first)//(aCollection size - 1).
	aCollection ~= newInterval
		ifTrue: [self error: 'The argument is not an arithmetic progression'].
	^newInterval.
