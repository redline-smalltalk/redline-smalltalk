"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger,
 LargePositiveInteger, and LargeNegativeInteger.

Integer division consists of:
	/	exact division, answers a fraction if result is not a whole integer
	//	answers an Integer, rounded towards negative infinity
	\\	is modulo rounded towards negative infinity
	quo: truncated division, rounded towards zero"

Number subclass: #Integer
	   category: 'Numeric-Numbers'.

"Integer methodsFor: 'testing'"

- benchFib  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t | t _ Time millisecondsToRun: [r _ 26 benchFib].
			r//t*1000 "
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1]
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1].

- benchmark  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
    | size flags prime k count |
    size := 8190.
    1 to: self do:
        [:iter |
        count := 0.
        flags := (Array new: size) atAllPut: true.
        1 to: size do:
            [:i | (flags at: i) ifTrue:
                [prime := i+1.
                k := i + prime.
                [k <= size] whileTrue:
                    [flags at: k put: false.
                    k := k + prime].
                count := count + 1]]].
    ^ count.

- even
	"Refer to the comment in Number|even."

	^((self digitAt: 1) bitAnd: 1) = 0.

- isInteger
	"True for all subclasses of Integer."

	^ true.

- isPowerOfTwo
	^ (self bitAnd: self-1) = 0.

"Integer methodsFor: 'arithmetic'"

- * aNumber
	"Refer to the comment in Number * "
	aNumber isInteger
		ifTrue: [^ self digitMultiply: aNumber
					neg: self negative ~~ aNumber negative]
		ifFalse: [^ (aNumber adaptInteger: self) * aNumber adaptToInteger].

- + aNumber
	"Refer to the comment in Number + "
	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [^(self digitAdd: aNumber) normalize]
					ifFalse: [^self digitSubtract: aNumber]]
		ifFalse: [^ (aNumber adaptInteger: self) + aNumber adaptToInteger].

- - aNumber
	"Refer to the comment in Number - "
	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [^ self digitSubtract: aNumber]
					ifFalse: [^ (self digitAdd: aNumber) normalize]]
		ifFalse: [^ (aNumber adaptInteger: self) - aNumber adaptToInteger].

- / aNumber
	"Refer to the comment in Number / "
	| quoRem |
	aNumber isInteger
		ifTrue: [quoRem := self digitDiv: aNumber
								neg: self negative ~~ aNumber negative.
				(quoRem at: 2) = 0
					ifTrue: [^(quoRem at: 1) normalize]
					ifFalse: [^(Fraction numerator: self denominator: aNumber) reduced]]
		ifFalse: [^ (aNumber adaptInteger: self) / aNumber adaptToInteger].

- // aNumber

	| q |
	aNumber = 0 ifTrue: [^self error: 'division by 0'].
	self = 0 ifTrue: [^0].
	q := self quo: aNumber
	"Refer to the comment in Number|//.".
	(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
		ifTrue: [^q - 1"Truncate towards minus infinity"]
		ifFalse: [^q].

- alignedTo: anInteger
	"Answer the smallest number not less than receiver that is a multiple of anInteger."

	^(self+anInteger-1//anInteger)*anInteger.

- quo: aNumber
	"Refer to the comment in Number quo: "
	| ng quo |
	aNumber isInteger
		ifTrue:
			[ng := self negative == aNumber negative == false.
			quo := (self digitDiv: aNumber neg: ng) at: 1.
			^ quo normalize]
		ifFalse: [^ (aNumber adaptInteger: self) quo: aNumber adaptToInteger].

"Integer methodsFor: 'comparing'"

- < aNumber
	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [self negative
								ifTrue: [^(self digitCompare: aNumber) > 0]
								ifFalse: [^(self digitCompare: aNumber) < 0]]
					ifFalse: [^self negative]]
		ifFalse: [^ (aNumber adaptInteger: self) < aNumber adaptToInteger].

- = aNumber
	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger
		ifTrue: [aNumber negative == self negative
					ifTrue: [^ (self digitCompare: aNumber) = 0]
					ifFalse: [^ false]]
		ifFalse: [^ (aNumber adaptInteger: self) = aNumber adaptToInteger].

- > aNumber
	aNumber isInteger
		ifTrue: [self negative == aNumber negative
					ifTrue: [self negative
								ifTrue: [^(self digitCompare: aNumber) < 0]
								ifFalse: [^(self digitCompare: aNumber) > 0]]
					ifFalse: [^ aNumber negative]]
		ifFalse: [^ (aNumber adaptInteger: self) > aNumber adaptToInteger].

- hash
	"Hash is reimplemented because = is implemented."

	^(self lastDigit bitShift: 8) + (self digitAt: 1).


- atRandom
	"Return a random integer from 1 to self.  Heavy users should use Interval atRandom or atRandom: directly."
	^ (1 to: self) atRandom.

- ceiling
	"Refer to the comment in Number|ceiling."
	^ self.

- floor
	"Refer to the comment in Number|floor."
	^ self.

- normalize
	"SmallInts OK; LgInts override"
	^ self.

- rounded
	"Refer to the comment in Number|rounded."
	^ self.

- truncated
	"Refer to the comment in Number|truncated."
	^ self.


"Integer methodsFor: 'enumerating'"

- timesRepeat: aBlock
	"Evaluate the argument, aBlock, the number of times represented by the
	receiver."

	| count |
	count := 1.
	[count <= self]
		whileTrue:
			[aBlock value.
			count := count + 1].


"Integer methodsFor: 'mathematical functions'"

- degreeCos
	"Answer the cosine of the receiver taken as an angle in degrees."

	^ (90 + self) degreeSin.

- degreeSin
	"Answer the sine of the receiver taken as an angle in degrees."

	^ self asFloat degreesToRadians sin.

- factorial
	"Answer the factorial of the receiver."

	self = 0 ifTrue: [^ 1].
	self > 0 ifTrue: [^ self * (self - 1) factorial].
	self error: 'Not valid for negative integers'.

- gcd: anInteger
	"See Knuth, Vol 2, 4.5.2, Algorithm L"
	"Initialize"
	| higher u v k uHat vHat a b c d vPrime vPrimePrime q t |
	higher := SmallInteger maxVal highBit.
	u := self abs max: (v := anInteger abs).
	v := self abs min: v.
	[u class == SmallInteger]
		whileFalse:
			[(uHat := u bitShift: (k := higher - u highBit)) class == SmallInteger
				ifFalse:
					[k := k - 1.
					uHat := uHat bitShift: -1].
			vHat := v bitShift: k.
			a := 1.
			b := 0.
			c := 0.
			d := 1.
			"Test quotient"
			[(vPrime := vHat + d) ~= 0
				and: [(vPrimePrime := vHat + c) ~= 0 and: [(q := uHat + a // vPrimePrime) = (uHat + b // vPrime)]]]
				whileTrue:
					["Emulate Euclid"
					c := a - (q * (a := c)).
					d := b - (q * (b := d)).
					vHat := uHat - (q * (uHat := vHat))].
			"Multiprecision step"
			b = 0
				ifTrue:
					[v := u rem: (u := v)]
				ifFalse:
					[t := u * a + (v * b).
					v := u * c + (v * d).
					u := t]].
	^ u gcd: v.

- lcm: n
	"Answer the least common multiple of the receiver and n."

	^self // (self gcd: n) * n.

- take: kk
	"Return the number of combinations of (self) elements taken kk at a time.  For 6 take 3, this is 6*5*4 / (1*2*3).  Zero outside of Pascal's triangle.  Use a trick to go faster."
	" 6 take: 3  "

	| num denom |
	kk < 0 ifTrue: [^ 0].
	kk > self ifTrue: [^ 0].
	num := 1.
	self to: (kk max: self-kk) + 1 by: -1 do: [:factor | num := num * factor].
	denom := 1.
	1 to: (kk min: self-kk) do: [:factor | denom := denom * factor].
	^ num // denom.


"Integer methodsFor: 'bit manipulation'"

- << shiftAmount  "left shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount.

- >> shiftAmount  "left shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount.

- allMask: mask
	"Treat the argument as a bit mask. Answer whether all of the bits that
	are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask).

- anyMask: mask
	"Treat the argument as a bit mask. Answer whether any of the bits that
	are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask).

- bitAnd: n
	"Answer an Integer whose bits are the logical AND of the receiver's bits
	and those of the argument, n."
	| norm |
	norm := n normalize.
	^ self digitLogic: norm
		op: #bitAnd:
		length: (self digitLength max: norm digitLength).

- bitClear: aMask
	"Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask."

	^ (self bitOr: aMask) - aMask.

- bitInvert
	"Answer an Integer whose bits are the logical negation of the receiver's bits.
	Numbers are interpreted as having 2's-complement representation."

	^ -1 - self.

- bitInvert32
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF.

- bitOr: n
	"Answer an Integer whose bits are the logical OR of the receiver's bits
	and those of the argument, n."
	| norm |
	norm := n normalize.
	^self digitLogic: norm
		op: #bitOr:
		length: (self digitLength max: norm digitLength).

- bitShift: shiftCount
	"Answer an Integer whose value (in twos-complement representation) is
	the receiver's value (in twos-complement representation) shifted left by
	the number of bits indicated by the argument. Negative arguments shift
	right. Zeros are shifted in from the right in left shifts."
	| rShift |
	shiftCount >= 0 ifTrue: [^ self digitLshift: shiftCount].
	rShift := 0 - shiftCount.
	^ (self digitRshift: (rShift bitAnd: 7)
				bytes: (rShift bitShift: -3)
				lookfirst: self digitLength) normalize.

- bitXor: n
	"Answer an Integer whose bits are the logical XOR of the receiver's bits
	and those of the argument, n."
	| norm |
	norm := n normalize.
	^self
		digitLogic: norm
		op: #bitXor:
		length: (self digitLength max: norm digitLength).

- lowBit
	"Answer the index of the low order bit of this number."
	| index |
	self = 0 ifTrue: [ ^ 0 ].
	index := 1.
	[ (self digitAt: index) = 0 ]
		whileTrue:
			[ index := index + 1 ].
	^ (self digitAt: index) lowBit + (8 * (index - 1)).

- noMask: mask
	"Treat the argument as a bit mask. Answer whether none of the bits that
	are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask).


"Integer methodsFor: 'converting'"

- adaptFloat: aFloat
	"If I am involved in arithmetic with a Float, do not convert the Float."
	^ aFloat.

- adaptFraction: aFraction
	"If I am involved in arithmetic with a Fraction, do not convert the Fraction."
	^ aFraction.

- adaptToFloat
	"If I am involved in arithmetic with a Float, convert me to a Float."
	^ self asFloat.

- adaptToFraction
	"If I am involved in arithmetic with a Fraction, convert me to a Fraction."
	^ self asFraction.

- asCharacter
	"Answer the Character whose value is the receiver."

	^Character value: self.

- asFloat
	"Answer a Float that represents the value of the receiver. Optimized to process only the significant digits of a LargeInteger."

	| sum factor numBytes |
	sum := 0.0.
	factor := self sign asFloat.
	numBytes := self size.
	numBytes > 7
		ifFalse: [
			1 to: self size do: [:i |
				sum := sum + ((self digitAt: i) * factor).
				factor := factor * 256.0]]
		ifTrue: [
			(numBytes - 6) to: numBytes do: [:i |
				sum := sum + ((self digitAt: i) * factor).
				factor := factor * 256.0].
			sum := sum timesTwoPower: 8 * (numBytes - 7)].
	^ sum.

- asFraction
	"Answer a Fraction that represents value of the the receiver."

	^Fraction numerator: self denominator: 1.

- asInteger
	"Answer with the receiver itself."

	^self.


"Integer methodsFor: 'printing'"

- asStringWithCommas  "123456789 asStringWithCommas"
	| digits |
	digits := self abs printString.
	^ String streamContents:
		[:strm | 1 to: digits size do:
			[:i | strm nextPut: (digits at: i).
			(i < digits size and: [(i - digits size) \\ 3 = 0])
				ifTrue: [strm nextPut: $,]]].

- hex
	^ self printStringBase: 16.

- hex8  "16r3333 hex8"
	| hex |
	hex := self hex.  "16rNNN"
	hex size < 11
		ifTrue: [^ hex copyReplaceFrom: 4 to: 3
						 with: ('00000000' copyFrom: 1 to: 11-hex size)]
		ifFalse: [^ hex].

- isLiteral

	^true.

"Integer methodsFor: 'printing'"

printOn: aStream base: b
	"Print a representation of the receiver on the stream, aStream, in base, b,
	where 2<=b<=16."
	| digits source dest i j pos t rem |
	b = 10 ifFalse: [aStream print: b; nextPut: $r].
	i := self digitLength.
	"Estimate size of result, conservatively"
	digits := Array new: i * 8.
	pos := 0.
	dest := i <= 1
		ifTrue: [self]
		ifFalse: [LargePositiveInteger new: i].
	source := self.
	[i >= 1]
		whileTrue:
			[rem := 0.
			j := i.
			[j > 0]
				whileTrue:
					[t := (rem bitShift: 8) + (source digitAt: j).
					dest digitAt: j put: t // b.
					rem := t \\ b.
					j := j - 1].
			pos := pos + 1.
			digits at: pos put: rem.
			source := dest.
			(source digitAt: i) = 0 ifTrue: [i := i - 1]].
	"(dest digitAt: 1) printOn: aStream base: b."
	[pos > 0]
		whileTrue:
			[aStream nextPut: (Character digitValue: (digits at: pos)).
			pos := pos - 1].

- radix: radix
	"Answer a String representing the receiver as a base radix integer."

	^ self printStringBase: radix.

- stringForReadout
	^ self rounded printString.


"Integer methodsFor: 'system primitives'"

- lastDigit
	"Answer the last digit of the integer."

	^self digitAt: self digitLength.

- replaceFrom: start to: stop with: replacement startingAt: repStart
	| j |  "Catches failure if LgInt replace primitive fails"
	j := repStart.
	start to: stop do:
		[:i |
		self digitAt: i put: (replacement digitAt: j).
		j := j+1].


"Integer methodsFor: 'private'"

- copyto: x
	| stop |
	stop := self digitLength min: x digitLength.
	^ x replaceFrom: 1 to: stop with: self startingAt: 1.

- digitAdd: arg
	| len arglen accum sum |
	accum := 0.
	(len := self digitLength) < (arglen := arg digitLength)
		ifTrue: [len := arglen].
	"Open code max: for speed"
	sum := Integer new: len neg: self negative.
	1 to: len do:
		[:i |
		accum := (accum bitShift: -8) + (self digitAt: i) + (arg digitAt: i).
		sum digitAt: i put: (accum bitAnd: 255)].
	accum > 255 ifTrue:
			[sum := sum growby: 1.
			sum at: sum digitLength put: (accum bitShift: -8)].
	^sum.

- digitCompare: arg
	"Compare the magnitude of self with that of arg.
	Return a code of 1, 0, -1 for self >, = , < arg"
	| len arglen argDigit selfDigit |
	len := self digitLength.
	(arglen := arg digitLength) ~= len
		ifTrue: [arglen > len
					ifTrue: [^-1]
					ifFalse: [^1]].
	[len > 0]
		whileTrue:
			[(argDigit := arg digitAt: len) ~= (selfDigit := self digitAt: len)
				ifTrue: [argDigit < selfDigit
							ifTrue: [^1]
							ifFalse: [^-1]].
			len := len - 1].
	^0.

- digitDiv: arg neg: ng
	"Answer with an array of (quotient, remainder)."
	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	l := self digitLength - arg digitLength + 1.
	l <= 0 ifTrue: [^Array with: 0 with: self].
	d := 8 - arg lastDigit highBit.
	div := arg digitLshift: d.  div := div growto: div digitLength + 1.
	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem digitLength = self digitLength ifTrue:
		[rem := rem growto: self digitLength + 1].
	"makes a copy and shifts"
	quo := Integer new: l neg: ng.
	dl := div digitLength - 1.
	"Last actual byte of data"
	ql := l.
	dh := div digitAt: dl.
	dnh := dl = 1
			ifTrue: [0]
			ifFalse: [div digitAt: dl - 1].
	1 to: ql do:
		[:k |
		"maintain quo*arg+rem=self"
		"Estimate rem/div by dividing the leading to bytes of rem by dh."
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
		j := rem digitLength + 1 - k.
		"r1 := rem digitAt: j."
		(rem digitAt: j) = dh
			ifTrue: [qhi := qlo := 15"i.e. q=255"]
			ifFalse:
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.
				Note that r1,r2 are bytes, not nibbles.
				Be careful not to generate intermediate results exceeding 13 bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem digitAt: j) bitShift: 4) + ((rem digitAt: j - 1) bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4) + ((rem digitAt: j - 1) bitAnd: 15).
				qlo := t // dh.
				t := t \\ dh.
				"Next compute (hi,lo) := q*dnh"
				hi := qhi * dnh.
				lo := qlo * dnh + ((hi bitAnd: 15) bitShift: 4).
				hi := (hi bitShift: -4) + (lo bitShift: -8).
				lo := lo bitAnd: 255.
				"Correct overestimate of q.
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3 ifTrue: [0]
						 ifFalse: [rem digitAt: j - 2].
				[(t < hi or: [t = hi and: [r3 < lo]]) and:
						["i.e. (t,r3) < (hi,lo)"
						qlo := qlo - 1.
						lo := lo - dnh.
						lo < 0 ifTrue:
								[hi := hi - 1.
								lo := lo + 256].
						hi >= dh]]
					whileTrue: [hi := hi - dh].
				qlo < 0
					ifTrue:
						[qhi := qhi - 1.
						qlo := qlo + 16]].
		"Subtract q*div from rem"
		l := j - dl.
		a := 0.
		1 to: div digitLength do:
			[:i |
			hi := (div digitAt: i) * qhi.
			lo := a + (rem digitAt: l)
					- ((hi bitAnd: 15) bitShift: 4)
					- ((div digitAt: i) * qlo).
			rem digitAt: l
				put: lo - (lo // 256 * 256) "sign-tolerant form of (lo bitAnd: 255)".
			a := (lo // 256) - (hi bitShift: -4).
			l := l + 1].
		a < 0 ifTrue:
				["Add div back into rem, decrease q by 1"
				qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div digitLength do:
					[:i |
					a := (a bitShift: -8) + (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l := l + 1]].
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4) + qlo].
	rem := rem digitRshift: d bytes: 0 lookfirst: dl.
	^Array with: quo with: rem.

- digitLogic: arg op: op length: len
	| result neg1 neg2 rneg z1 z2 rz b1 b2 b |
	neg1 := self negative.
	neg2 := arg negative.
	rneg :=
		((neg1 ifTrue: [-1] ifFalse: [0])
			perform: op
			with: (neg2
					ifTrue: [-1]
					ifFalse: [0])) < 0.
	result := Integer new: len neg: rneg.
	rz := z1 := z2 := true.
	1 to: result digitLength do:
		[:i |
		b1 := self digitAt: i.
		neg1
			ifTrue: [b1 := z1
						ifTrue: [b1 = 0
									ifTrue: [0]
									ifFalse:
										[z1 := false.
										256 - b1]]
						ifFalse: [255 - b1]].
		b2 := arg digitAt: i.
		neg2
			ifTrue: [b2 := z2
						ifTrue: [b2 = 0
									ifTrue: [0]
									ifFalse:
										[z2 := false.
										256 - b2]]
						ifFalse: [255 - b2]].
		b := b1 perform: op with: b2.
		b = 0
			ifTrue:
				[result digitAt: i put: 0]
			ifFalse:
				[result
					digitAt: i
					put: (rneg
							ifTrue: [rz ifTrue:
											[rz := false.
											256 - b]
										ifFalse: [255 - b]]
						ifFalse: [b])]].
	^ result normalize.

- digitLshift: shiftCount
	| carry rShift mask len result digit byteShift bitShift highBit |
	(highBit := self highBit) = 0 ifTrue: [^ 0].
	len := highBit + shiftCount + 7 // 8.
	result := Integer new: len neg: self negative.
	byteShift := shiftCount // 8.
	bitShift := shiftCount \\ 8.
	bitShift = 0 ifTrue:
		["Fast version for byte-aligned shifts"
		^ result replaceFrom: byteShift+1 to: len
				with: self startingAt: 1].
	carry := 0.
	rShift := bitShift - 8.
	mask := 255 bitShift: 0 - bitShift.
	1 to: byteShift do: [:i | result digitAt: i put: 0].
	1 to: len - byteShift do:
		[:i |
		digit := self digitAt: i.
		result digitAt: i + byteShift
			put: (((digit bitAnd: mask) bitShift: bitShift) bitOr: carry).
		carry := digit bitShift: rShift].
	^ result.

- digitMultiply: arg neg: ng
	| prod prodLen carry digit k ab |
	(arg digitLength = 1 and: [(arg digitAt: 1) = 0]) ifTrue: [^ 0].
	prodLen := self digitLength + arg digitLength.
	prod := Integer new: prodLen neg: ng.
	"prod starts out all zero"
	1 to: self digitLength do:
		[:i |
		(digit := self digitAt: i) ~= 0
			ifTrue:
				[k := i.
				carry := 0.
				"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do:
					[:j |
					ab := ((arg digitAt: j) * digit) + carry
							+ (prod digitAt: k).
					carry := ab bitShift: -8.
					prod digitAt: k put: (ab bitAnd: 255).
					k := k + 1].
				prod digitAt: k put: carry]].
	^ prod normalize.

- digitRshift: anInteger bytes: b lookfirst: a
	 "Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."
	| n x r f m digit count i |
	n := 0 - anInteger.
	x := 0.
	f := n + 8.
	i := a.
	m := 255 bitShift: 0 - f.
	digit := self digitAt: i.
	[((digit bitShift: n) bitOr: x) = 0 and: [i ~= 1]] whileTrue:
		[x := digit bitShift: f "Can't exceed 8 bits".
		i := i - 1.
		digit := self digitAt: i].
	i <= b ifTrue: [^Integer new: 0 neg: self negative].  "All bits lost"
	r := Integer new: i - b neg: self negative.
	count := i.
	x := (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do:
		[:j | digit := self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x)
			"Avoid values > 8 bits".
		x := digit bitShift: n].
	^r.

- digitSubtract: arg
	| smaller larger z sum sl al ng |
	sl := self digitLength.
	al := arg digitLength.
	(sl = al
		ifTrue:
			[[(self digitAt: sl) = (arg digitAt: sl) and: [sl > 1]]
				whileTrue: [sl := sl - 1].
			al := sl.
			(self digitAt: sl) < (arg digitAt: sl)]
		ifFalse: [sl < al])
		ifTrue:
			[larger := arg.
			smaller := self.
			ng := self negative == false.
			sl := al]
		ifFalse:
			[larger := self.
			smaller := arg.
			ng := self negative].
	sum := Integer new: sl neg: ng.
	z := 0.
	"Loop invariant is -1<=z<=1"
	1 to: sl do:
		[:i |
		z := z + (larger digitAt: i) - (smaller digitAt: i).
		sum digitAt: i
			put: z - (z // 256 * 256) "sign-tolerant form of (z bitAnd: 255)".
		z := z // 256].
	^ sum normalize.

- growby: n

	^self growto: self digitLength + n.

- growto: n

	^self copyto: (self species new: n).

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Integer class methodsFor: 'instance creation'"

+ byte1: byte1 byte2: byte2 byte3: byte3 byte4: byte4
	"Depending on high-order byte copy directly into a LargeInteger,
	or build up a SmallInteger by shifting"
	| value |
	byte4 < 16r20 ifTrue:
		[^ (byte4 bitShift: 24)
		 + (byte3 bitShift: 16)
		 + (byte2 bitShift: 8)
		 + byte1].
	value := LargePositiveInteger new: 4.
	value digitAt: 4 put: byte4.
	value digitAt: 3 put: byte3.
	value digitAt: 2 put: byte2.
	value digitAt: 1 put: byte1.
	^ value.

+ new: length neg: neg
	"Answer an instance of a large integer whose size is length. neg is a flag
	determining whether the integer is negative or not."

	neg
		ifTrue: [^LargeNegativeInteger new: length]
		ifFalse: [^LargePositiveInteger new: length].

+ primesUpTo: max
	"Return a list of prime integers up to the given integer."
	"Integer primesUpTo: 100"

	| out limit flags prime k |
	out := OrderedCollection new.
	limit := max asInteger - 1.
	flags := (Array new: limit) atAllPut: true.
	1 to: limit do: [:i |
		(flags at: i) ifTrue: [
			prime := i + 1.
			k := i + prime.
			[k <= limit] whileTrue: [
				flags at: k put: false.
				k := k + prime].
			out add: prime]].
	^ out asArray.

+ readFrom: aStream
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStream base: 10.

+ readFrom: aStream base: base
	"Answer an instance of one of my concrete subclasses. Initial minus sign
	accepted, and bases > 10 use letters A-Z. Embedded radix specifiers not
	allowed--use Number readFrom: for that. Answer zero (not an error) if
	there are no digits."

	| digit value neg |
	neg := aStream peekFor: $-.
	value := 0.
	[aStream atEnd]
		whileFalse:
			[digit := aStream next digitValue.
			(digit < 0 or: [digit >= base])
				ifTrue:
					[aStream skip: -1.
					neg ifTrue: [^ value negated].
					^ value]
				ifFalse: [value := value * base + digit]].
	neg ifTrue: [^ value negated].
	^ value.

+ testBitLogic  "Integer testBitLogic"
	"This little suite of tests is designed to verify correct operation of most
	of Squeak's bit manipulation code, including two's complement
	representation of negative values.  It was written in a hurry and
	is probably lacking several important checks."

	"Shift 1 bit left then right and test for 1"
	| n |
	1 to: 100 do: [:i | ((1 bitShift: i) bitShift: i negated) = 1 ifFalse: [self error: 'Bit Logic Failure']].

	"Shift -1 left then right and test for 1"
	1 to: 100 do: [:i | ((-1 bitShift: i) bitShift: i negated) = -1 ifFalse: [self error: 'Bit Logic Failure']].

	"And a single bit with -1 and test for same value"
	1 to: 100 do: [:i | ((1 bitShift: i) bitAnd: -1) = (1 bitShift: i) ifFalse: [self error: 'Bit Logic Failure']].

	"Verify that (n bitAnd: n negated) = n for single bits"
	1 to: 100 do: [:i | n := 1 bitShift: i.
				(n bitAnd: n negated) = n ifFalse: [self error: 'Bit Logic Failure']].

	"Verify that n negated = (n complemented + 1) for single bits"
	1 to: 100 do: [:i | n := 1 bitShift: i.
				n negated = ((n bitXor: -1) + 1) ifFalse: [self error: 'Bit Logic Failure']].

	"Verify that (n + n complemented) = -1 for single bits"
	1 to: 100 do: [:i | n := 1 bitShift: i.
				(n + (n bitXor: -1)) = -1 ifFalse: [self error: 'Bit Logic Failure']].

	"Verify that n negated = (n complemented +1) for single bits"
	1 to: 100 do: [:i | n := 1 bitShift: i.
				n negated = ((n bitXor: -1) + 1) ifFalse: [self error: 'Bit Logic Failure']].

