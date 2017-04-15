"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent some rational number as a fraction. All public arithmetic operations answer reduced fractions."

Number subclass: #Fraction
	   instanceVariableNames: 'numerator denominator '
	   category: 'Numeric-Numbers'.

"Fraction methodsFor: 'arithmetic'"

- * aNumber
	"Answer the result of multiplying the receiver by aNumber."
	| d1 d2 |
	aNumber isFraction
		ifTrue:
			[d1 := numerator gcd: aNumber denominator.
			d2 := denominator gcd: aNumber numerator.
			(d2 = denominator and: [d1 = aNumber denominator])
				ifTrue: [^ numerator // d1 * (aNumber numerator // d2)].
			^ Fraction numerator: numerator // d1 * (aNumber numerator // d2) denominator: denominator // d2 * (aNumber denominator // d1)]
		ifFalse: [^ (aNumber adaptFraction: self)
				* aNumber adaptToFraction].

- + aNumber
	"Answer the sum of the receiver and aNumber."
	| n d d1 d2 |
	aNumber isFraction
		ifTrue:
			[d := denominator gcd: aNumber denominator.
			n := numerator * (d1 := aNumber denominator // d) + (aNumber numerator * (d2 := denominator // d)).
			d1 := d1 * d2.
			n := n // (d2 := n gcd: d).
			(d := d1 * (d // d2)) = 1 ifTrue: [^ n].
			^ Fraction numerator: n denominator: d]
		ifFalse: [^ (aNumber adaptFraction: self)
				+ aNumber adaptToFraction].

- - aNumber
	"Answer the difference between the receiver and aNumber."
	aNumber isFraction
		ifTrue: [^ self + aNumber negated]
		ifFalse: [^ (aNumber adaptFraction: self) - aNumber adaptToFraction].

- / aNumber
	"Answer the result of dividing the receiver by aNumber."
	aNumber isFraction
		ifTrue: [^self * aNumber reciprocal]
		ifFalse: [^ (aNumber adaptFraction: self) / aNumber adaptToFraction].

- negated
	"Refer to the comment in Number|negated."

	^ Fraction
		numerator: numerator negated
		denominator: denominator.

- reciprocal
	"Refer to the comment in Number|reciprocal."

	numerator = 0 ifTrue: [self error: '0 has no reciprocal'].
	numerator = 1 ifTrue: [^denominator].
	numerator = -1 ifTrue: [^denominator negated].
	^Fraction numerator: denominator denominator: numerator.


"Fraction methodsFor: 'comparing'"

- < aNumber
	aNumber isFraction
		ifTrue: [^ numerator * aNumber denominator < (aNumber numerator * denominator)]
		ifFalse: [^ (aNumber adaptFraction: self)
					< aNumber adaptToFraction].


- = aNumber
	aNumber isNumber ifFalse: [^ false].
	aNumber isFraction
		ifTrue: [aNumber numerator = 0
				ifTrue: [^numerator = 0]
				ifFalse: [^aNumber numerator = numerator
							and: [aNumber denominator = denominator]]]
		ifFalse: [^ (aNumber adaptFraction: self) = aNumber adaptToFraction].

- hash
	"Hash is reimplemented because = is implemented."

	^numerator bitXor: denominator.

"Fraction methodsFor: 'truncation and round off'"

- truncated
	"Refer to the comment in Number|truncated."

	^numerator quo: denominator.

"Fraction methodsFor: 'converting'"

adaptFloat: aFloat
	"If I am involved in arithmetic with a Float, do not convert the Float."
	^ aFloat.

- adaptInteger: anInteger
	"If I am involved in arithmetic with an Integer, convert the Integer."
	^ anInteger asFraction.

- adaptToFloat
	"If I am involved in arithmetic with a Float, convert me to a Float."
	^ self asFloat.

- adaptToInteger
	"If I am involved in arithmetic with an Integer, do not convert me."
	^ self.

- asFloat
	"Answer a Float that represents the same value as does the receiver."

	| df scaleFactor scaledDenominator |
	df := denominator asFloat.
	^ df isInfinite
		ifTrue:
			["might be representable as a denormalized Float"
			scaleFactor := 2 raisedToInteger: 53.
			scaledDenominator := (denominator / scaleFactor) rounded.
			numerator asFloat / scaleFactor asFloat / scaledDenominator asFloat]
		ifFalse:
			[numerator asFloat / df].

- asFraction
	"Answer the receiver itself."

	^ self.

- isFraction
	^ true.

"Fraction methodsFor: 'printing'"

- printOn: aStream

	aStream nextPut: $(.
	numerator printOn: aStream.
	aStream nextPut: $/.
	denominator printOn: aStream.
	aStream nextPut: $).


"Fraction methodsFor: 'private'"

- denominator

	^ denominator.

- numerator

	^ numerator.

- reduced

	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd := numerator gcd: denominator.
	numer := numerator // gcd.
	denom := denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom.

- setNumerator: n denominator: d

	d = 0
		ifTrue: [self error: 'denominator cannot be zero']
		ifFalse:
			[numerator := n asInteger.
			denominator := d asInteger abs. "keep sign in numerator"
			d < 0 ifTrue: [numerator := numerator negated]].


"Fraction methodsFor: 'mathematical functions'"

- raisedToInteger: anInteger
	"See Number | raisedToInteger:"
	anInteger = 0 ifTrue: [^ 1].
	anInteger < 0 ifTrue: [^ self reciprocal raisedToInteger: anInteger negated].
	^ Fraction numerator: (numerator raisedToInteger: anInteger)
		denominator: (denominator raisedToInteger: anInteger).

- squared
	"See Fraction (Number) | squared"
	^ Fraction numerator: numerator squared denominator: denominator squared.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Fraction class methodsFor: 'instance creation'"

+ numerator: numInteger denominator: denInteger
	"Answer an instance of me (denInteger/numInteger)."

	^ self new setNumerator: numInteger denominator: denInteger.
