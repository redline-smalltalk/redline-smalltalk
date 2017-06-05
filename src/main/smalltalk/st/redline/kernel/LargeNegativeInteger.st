"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"Just like LargePositiveInteger, but represents a negative number."

LargePositiveInteger subclass: #LargeNegativeInteger
                     category: 'Numeric-Numbers'.
                     
"LargeNegativeInteger methodsFor: 'arithmetic'"

- abs
	^ self negated.

- negated
	^ self copyto: (LargePositiveInteger new: self digitLength).


"LargeNegativeInteger methodsFor: 'converting'"

- normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen minVal |
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len := len - 1].

	"Now check if in SmallInteger range"
	sLen := 4  "SmallInteger minVal digitLength".
	len <= sLen ifTrue:
		[minVal := SmallInteger minVal.
		(len < sLen
			or: [(self digitAt: sLen) < minVal lastDigit])
			ifTrue: ["If high digit less, then can be small"
					val := 0.
					len to: 1 by: -1 do:
						[:i | val := (val *256) - (self digitAt: i)].
					^ val].
		1 to: sLen do:  "If all digits same, then = minVal"
			[:i | (self digitAt: i) = (minVal digitAt: i)
					ifFalse: ["Not so; return self shortened"
							len < oldLen
								ifTrue: [^ self growto: len]
								ifFalse: [^ self]]].
		^ minVal].

	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [^ self growto: len]
		ifFalse: [^ self].


"LargeNegativeInteger methodsFor: 'testing'"

- negative
	"Answer whether the receiver is mathematically negative."

	^ true.

- positive
	"Answer whether the receiver is positive or equal to 0. (ST-80 protocol).
	See also strictlyPositive"

	^ false.

- sign
	"Optimization. Answer -1 since receiver is less than 0."

	^ -1.

- strictlyPositive
	"Answer whether the receiver is mathematically positive."

	^ false.

"LargeNegativeInteger methodsFor: 'printing'"

- printOn: aStream base: b
	"Refer to the comment in Integer|printOn:base:."

	aStream nextPut: $-.
	super printOn: aStream base: b.
