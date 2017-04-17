"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a positive large integer, integers greater than 2-to-the-30th (1073741824).  These are beyond the range of
 SmallInteger, encoded as an array of 8-bit digits.  Care must be taken, when new results are computed, that any value
 that COULD BE a SmallInteger IS a SmallInteger (see normalize)."

Integer subclass: #LargePositiveInteger
        category: 'Numeric-Numbers'.

"LargePositiveInteger methodsFor: 'arithmetic'"

- * anInteger
	"Primitive. Multiply the receiver by the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	JVM primitive: 29.

- + anInteger
	"Primitive. Add the receiver to the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	JVM primitive: 21.

- - anInteger
	"Primitive. Subtract the argument from the receiver and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	JVM primitive: 22.

- / anInteger
	"Primitive. Divide the receiver by the argument and answer with the
	result if the division is exact. Fail if the result is not a whole integer.
	Fail if the argument is 0. Fail if either the argument or the result is not
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	JVM primitive: 30.

- // anInteger
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards negative infinity to make it a whole
	integer. Fail if the argument is 0. Fail if either the argument or the
	result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive. "

	JVM primitive: 32.

- abs
    ^ self.

- negated
	^ (self copyto: (LargeNegativeInteger new: self digitLength))
		normalize.  "Need to normalize to catch SmallInteger minVal"

- quo: anInteger
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards zero to make it a whole integer. Fail if
	the argument is 0. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	JVM primitive: 33.

- \\ anInteger
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided
	by the argument. Fail if the argument is 0. Fail if either the argument
	or the result is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 31.

"LargePositiveInteger methodsFor: 'bit manipulation'"

- bitAnd: anInteger
	"Primitive. Answer an Integer whose bits are the logical AND of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."
	JVM primitive: 14.

- bitOr: anInteger
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."
	JVM primitive: 15.

- bitShift: anInteger
	"Primitive. Answer an Integer whose value (in twos-complement
	representation) is the receiver's value (in twos-complement
	representation) shifted left by the number of bits indicated by the
	argument. Negative arguments shift right. Zeros are shifted in from the
	right in left shifts. The sign bit is extended in right shifts.
	Fail if the receiver or result is greater than 32 bits.
	See Object documentation whatIsAPrimitive."
	JVM primitive: 17.

- bitXor: anInteger
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."
	JVM primitive: 16.

- highBit
	"Answer the index of the high order bit of the receiver, or zero if the receiver is zero. This method is allowed 
	(and needed) for LargeNegativeIntegers as well, since Squeak's LargeIntegers are sign/magnitude."
	| realLength lastDigit |
	realLength := self digitLength.
	[(lastDigit := self digitAt: realLength) = 0]
		whileTrue:
		[(realLength := realLength - 1) = 0 ifTrue: [^ 0]].
	^ lastDigit highBit + (8 * (realLength - 1)).

"LargePositiveInteger methodsFor: 'testing'"

- negative
	"Answer whether the receiver is mathematically negative."

	^ false.

- positive
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ true.

- sign
	"Optimization. Answer 1 since receiver is greater than 0."

	^ 1.

- strictlyPositive
	"Answer whether the receiver is mathematically positive."

	^ true.

"LargePositiveInteger methodsFor: 'comparing'"

- < anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 23.

- <= anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 25.

- = anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	receiver or argument is negative or greater than 32 bits.
	Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 7.

- > anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 24.

- >= anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger or a LargePositiveInteger
	less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 26.

- ~= anInteger
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	receiver or argument is negative or greater than 32 bits.
	Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 8.

"LargePositiveInteger methodsFor: 'converting'"

- normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen |
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range"
	sLen := SmallInteger maxVal digitLength.
	(len <= sLen
		and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val := 0.
				len to: 1 by: -1 do:
					[:i | val := (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self].


"LargePositiveInteger methodsFor: 'system primitives'"

- digitAt: index
	"Primitive. Answer the value of an indexable field in the receiver. Fail if
	the argument (the index) is not an Integer or is out of bounds. Essential.
	See Object documentation whatIsAPrimitive."

	JVM primitive: 60.

- digitAt: index put: value
	"Primitive. Store the second argument (value) in the indexable field of
	the receiver indicated by index. Fail if the value is negative or is larger
	than 255. Fail if the index is not an Integer or is out of bounds. Answer
	the value that was stored. Essential. See Object documentation
	whatIsAPrimitive."

	JVM primitive: 61.

- digitLength
	"Primitive. Answer the number of indexable fields in the receiver. This
	value is the same as the largest legal subscript. Essential. See Object
	documentation whatIsAPrimitive."

	JVM primitive: 62.

- replaceFrom: start to: stop with: replacement startingAt: repStart
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in
	the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional.
	See Object documentation whatIsAPrimitive."
	JVM primitive: 105.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"LargePositiveInteger class methodsFor: 'testing'"

+ test: n  "Time millisecondsToRun: [LargePositiveInteger test: 100] 1916"
	| f f1 |
	"Test and time mult, div, add, subtract"
	f := n factorial.
	f1 := f*(n+1).
	n timesRepeat: [f1 := f1 - f].
	f1 = f ifFalse: [self halt].
	n timesRepeat: [f1 := f1 + f].
	f1 // f = (n+1) ifFalse: [self halt].
	f1 negated = (Number readFrom: '-' , f1 printString) ifFalse: [self halt].

	"Check normalization and conversion to/from SmallInts"
	(SmallInteger maxVal + 1 - 1) == SmallInteger maxVal ifFalse: [self halt].
	(SmallInteger maxVal + 3 - 6) == (SmallInteger maxVal-3) ifFalse: [self halt].
	(SmallInteger minVal - 1 + 1) == SmallInteger minVal ifFalse: [self halt].
	(SmallInteger minVal - 3 + 6) == (SmallInteger minVal+3) ifFalse: [self halt].

	"Check bitShift from and back to SmallInts"
	1 to: 257 do: [:i | ((i bitShift: i) bitShift: 0-i) == i ifFalse: [self halt]].
