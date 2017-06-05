"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"My instances represent IEEE-754 floating-point double-precision numbers.  They have about 16 digits of accuracy and
 their range is between plus and minus 10^307. Some valid examples are:
 	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12
 Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point."

Number variableWordSubclass: #Float
	instanceVariableNames: ''
	classVariableNames: 'E Epsilon Halfpi Infinity Ln10 Ln2 MaxVal MaxValLn MinValLogBase2 NaN NegativeInfinity NegativeZero Pi RadiansPerDegree Sqrt2 Twopi '
	poolDictionaries: ''
	category: 'Numeric-Numbers'.

"Float methodsFor: 'arithmetic'"

- * aNumber
    "Primitive. Answer the result of multiplying the receiver by aNumber.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 49.

- + aNumber
    "Primitive. Answer the sum of the receiver and aNumber. Essential.
    Fail if the argument is not a Float. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 41.

- - aNumber
    "Primitive. Answer the difference between the receiver and aNumber.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 42.

- / aNumber
    "Primitive. Answer the result of dividing receiver by aNumber.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 50.

- abs
    "This is faster than using Number abs."
    self < 0.0
      ifTrue: [^ 0.0 - self]
      ifFalse: [^ self].

- negated
    "Answer a Number that is the negation of the receiver."

    ^ 0.0 - self.

- reciprocal
	^ 1.0 / self.


"Float methodsFor: 'mathematical functions'"

- arcCos
    "Answer the angle in radians."

    ^ Halfpi - self arcSin.

- arcSin
	"Answer the angle in radians."

	((self < -1.0) or: [self > 1.0]) ifTrue: [self error: 'Value out of range'].
	((self = -1.0) or: [self = 1.0])
		ifTrue: [^ Halfpi]
		ifFalse: [^ (self / (1.0 - (self * self)) sqrt) arcTan].

- arcTan
    "Answer the angle in radians.
     Optional. See Object documentation whatIsAPrimitive."

    JVM primitive: 57.

- cos
    "Answer the cosine of the receiver taken as an angle in radians."

    ^ (self + Halfpi) sin.

- degreeCos
    "Answer the cosine of the receiver taken as an angle in degrees."

    ^ self degreesToRadians cos.

- degreeSin
    "Answer the sine of the receiver taken as an angle in degrees."

    ^ self degreesToRadians sin.

- exp
    "Answer E raised to the receiver power.
     Optional. See Object documentation whatIsAPrimitive."

    JVM primitive: 59.

- floorLog: radix
    "Answer the floor of the log base radix of the receiver."

    ^ (self log: radix) floor.

- ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 58.

- log
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10.

- raisedTo: aNumber
	"Answer the receiver raised to aNumber."

	0.0 = aNumber ifTrue: [^ 1.0].  "special case for 0.0 raisedTo: 0.0"
	^ (self ln * aNumber asFloat) exp.

- reciprocalFloorLog: radix
    "Quick computation of (self log: radix) floor, when self < 1.0.
    Avoids infinite recursion problems with denormalized numbers"

    | adjust scale n |
    adjust := 0.
    scale := 1.0.
    [(n := radix / (self * scale)) isInfinite]
      whileTrue:
        [scale := scale * radix.
        adjust := adjust + 1].
    ^ ((n floorLog: radix) + adjust) negated.

- reciprocalLogBase2
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [Ln2 / Ln10]
		ifFalse: [Ln2 / self ln].

- sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 56.

- sqrt
	"Answer the square root of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	JVM primitive: 55.

- tan
    "Answer the tangent of the receiver taken as an angle in radians."

    ^ self sin / self cos.

- timesTwoPower: anInteger
    "Primitive. Answer with the receiver multiplied by 2.0 raised
    to the power of the argument.
    Optional. See Object documentation whatIsAPrimitive."

    JVM primitive: 54.


"Float methodsFor: 'comparing'"

- < aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is less than the argument. Otherwise return false.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 43.

"Float methodsFor: 'comparing'"

- <= aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is less than or equal to the argument. Otherwise return
    false. Fail if the argument is not a Float. Optional. See Object
    documentation whatIsAPrimitive."

    JVM primitive: 45.

- = aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is equal to the argument. Otherwise return false.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 47.

- > aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is greater than the argument. Otherwise return false.
    Fail if the argument is not a Float. Essential. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 44.

- >= aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is greater than or equal to the argument. Otherwise return
    false. Fail if the argument is not a Float. Optional. See Object documentation
    whatIsAPrimitive. "

    JVM primitive: 46.

- hash
    "Hash is reimplemented because = is implemented. Both words of the float are used; 8 bits are removed from each end
     to clear most of the exponent regardless of the byte ordering. (The bitAnd:'s ensure that the intermediate results
     do not become a large integer.) Slower than the original version in the ratios 12:5 to 2:1 depending on values."

    ^ (((self basicAt: 1) bitAnd: 16r00FFFF00) +
       ((self basicAt: 2) bitAnd: 16r00FFFF00)) bitShift: -8.

- ~= aNumber
    "Primitive. Compare the receiver with the argument and return true
    if the receiver is not equal to the argument. Otherwise return false.
    Fail if the argument is not a Float. Optional. See Object documentation
    whatIsAPrimitive."

    JVM primitive: 48.

"Float methodsFor: 'testing'"

- isFloat
	  ^ true.

- isInfinite
    "Return true if the receiver is positive or negative infinity."

    ^ self = Infinity or: [self = NegativeInfinity].

- isLiteral

	^ true.

- isNaN
    "simple, byte-order independent test for Not-a-Number"

    ^ self ~= self.

- sign
    "Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
    Handle IEEE-754 negative-zero by reporting a sign of -1"

    self > 0 ifTrue: [^ 1].
    (self < 0 or: [((self at: 1) bitShift: -31) = 1]) ifTrue: [^ -1].
    ^ 0.


"Float methodsFor: 'truncation and round off'"

- exponent
    "Primitive. Consider the receiver to be represented as a power of two
    multiplied by a mantissa (between one and two). Answer with the
    SmallInteger to whose power two is raised. Optional. See Object
    documentation whatIsAPrimitive."

	JVM primitive: 53.

- fractionPart
    "Primitive. Answer a Float whose value is the difference between the
    receiver and the receiver's asInteger value. Optional. See Object
    documentation whatIsAPrimitive."

    JVM primitive: 52.

- integerPart
    "Answer a Float whose value is the receiver's truncated value."

    ^ self - self fractionPart.

- rounded
    "Answer the integer nearest the receiver."

    self >= 0.0
      ifTrue: [^ (self + 0.5) truncated]
      ifFalse: [^ (self - 0.5) truncated].

- significand

	  ^ self timesTwoPower: (self exponent negated).

- significandAsInteger

    | exp sig |
    exp := self exponent.
    sig := (((self at: 1) bitAnd: 16r000FFFFF) bitShift: 32) bitOr: (self at: 2).
    exp > -1023
      ifTrue: [sig := sig bitOr: (1 bitShift: 52)].
    ^ sig.

- truncated
    "Answer with a SmallInteger equal to the value of the receiver without
    its fractional part. The primitive fails if the truncated value cannot be
    represented as a SmallInteger. In that case, the code below will compute
    a LargeInteger truncated value. Essential. See Object documentation
    whatIsAPrimitive. "

    JVM primitive: 51.


"Float methodsFor: 'converting'"

- adaptFraction: aFraction
    "If I am involved in arithmetic with a Fraction, convert the Fraction."
    ^ aFraction asFloat.

- adaptInteger: anInteger
    "If I am involved in arithmetic with an Integer, convert the Integer."
    ^ anInteger asFloat.

- adaptToFraction
    "If I am involved in arithmetic with a Fraction, do not convert me."
    ^ self.

- adaptToInteger
    "If I am involved in arithmetic with an Integer, do not convert me."
    ^ self.

- asApproximateFraction
    "Answer a Fraction approximating the receiver. This conversion uses the
    continued fraction method to approximate a floating point number."

    | num1 denom1 num2 denom2 int frac newD temp |
    num1 := self asInteger.	"The first of two alternating numerators"
    denom1 := 1.		"The first of two alternating denominators"
    num2 := 1.		"The second numerator"
    denom2 := 0.		"The second denominator--will update"
    int := num1.		"The integer part of self"
    frac := self fractionPart.		"The fractional part of self"
    [frac := 0]
      whileFalse:
        ["repeat while the fractional part is not zero"
        newD := 1.0 / frac.			"Take reciprocal of the fractional part"
        int := newD asInteger.		"get the integer part of this"
        frac := newD fractionPart.	"and save the fractional part for next time"
        temp := num2.				"Get old numerator and save it"
        num2 := num1.				"Set second numerator to first"
        num1 := num1 * int + temp.	"Update first numerator"
        temp := denom2.				"Get old denominator and save it"
        denom2 := denom1.			"Set second denominator to first"
        denom1 := int * denom1 + temp.		"Update first denominator"
        10000000000.0 < denom1
          ifTrue:
            ["Is ratio past float precision?  If so, pick which
            of the two ratios to use"
            num2 = 0.0
              ifTrue: ["Is second denominator 0?"
                  ^ Fraction numerator: num1 denominator: denom1].
            ^ Fraction numerator: num2 denominator: denom2]].
    "If fractional part is zero, return the first ratio"
    denom1 = 1
      ifTrue: ["Am I really an Integer?"
          ^ num1 "Yes, return Integer result"]
      ifFalse: ["Otherwise return Fraction result"
          ^ Fraction numerator: num1 denominator: denom1].

- asFloat
    "Answer the receiver itself."

    ^ self.

- asFraction
    ^ self asApproximateFraction.

- asTrueFraction
    " Answer a fraction that EXACTLY represents self,
      a double precision IEEE floating point number.
      Floats are stored in the same form on all platforms.
      (Does not handle gradual underflow or NANs.)
      By David N. Smith with significant performance
      improvements by Luciano Esteban Notarfrancesco.
      (Version of 11April97)"
    | shifty sign expPart exp fraction fractionPart result zeroBitsCount |
    self isInfinite ifTrue: [self error: 'Cannot represent infinity as a fraction'].
    self isNaN ifTrue: [self error: 'Cannot represent Not-a-Number as a fraction'].

    " Extract the bits of an IEEE double float "
    shifty := ((self basicAt: 1) bitShift: 32) + (self basicAt: 2).

    " Extract the sign and the biased exponent "
    sign := (shifty bitShift: -63) = 0 ifTrue: [1] ifFalse: [-1].
    expPart := (shifty bitShift: -52) bitAnd: 16r7FF.

    " Extract fractional part; answer 0 if this is a true 0.0 value "
    fractionPart := shifty bitAnd:  16r000FFFFFFFFFFFFF.
    ( expPart=0 and: [ fractionPart=0 ] ) ifTrue: [ ^ 0  ].

    " Replace omitted leading 1 in fraction "
    fraction := fractionPart bitOr: 16r0010000000000000.

    "Unbias exponent: 16r3FF is bias; 52 is fraction width"
    exp := 16r3FF + 52 - expPart.

    " Form the result. When exp>52, the exponent is adjusted by
      the number of trailing zero bits in the fraction to minimize
      the (huge) time otherwise spent in #gcd:. "
    exp negative
      ifTrue: [
        result := sign * fraction bitShift: exp negated ]
      ifFalse:	[
        zeroBitsCount _ fraction lowBit - 1.
        exp := exp - zeroBitsCount.
        exp <= 0
          ifTrue: [
            zeroBitsCount := zeroBitsCount + exp.
            "exp := 0."   " Not needed; exp not
  refernced again "
            result := sign * fraction bitShift:
  zeroBitsCount negated ]
          ifFalse: [
            result := Fraction
              numerator: (sign * fraction
  bitShift: zeroBitsCount negated)
              denominator: (1 bitShift:
  exp) ] ].

    " Validate the result (low cost; optional); answer result "
    (result asFloat = self)
      ifFalse: [self error: 'asTrueFraction validation failed' ].
    ^ result.

- degreesToRadians
    "Answer the receiver in radians. Assumes the receiver is in degrees."

    ^self * RadiansPerDegree.

- isInf
    "simple, byte-order independent test for +/- Infinity"

    ^ self = (self * 1.5 + 1.0).

- radiansToDegrees
    "Answer the receiver in degrees. Assumes the receiver is in radians."

    ^ self / RadiansPerDegree.


"Float methodsFor: 'copying'"

- deepCopy

    ^ self copy.

- shallowCopy

	  ^ self + 0.0.


"Float methodsFor: 'printing'

- absPrintExactlyOn: aStream base: base
    "Print my value on a stream in the given base.  Assumes that my value is strictly
    positive; negative numbers, zero, and NaNs have already been handled elsewhere.
    Based upon the algorithm outlined in:
    Robert G. Burger and R. Kent Dybvig
    Printing Floating Point Numbers Quickly and Accurately
    ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
    June 1996.
    This version guarantees that the printed representation exactly represents my value
    by using exact integer arithmetic."

    | fBase significand exp baseExpEstimate be be1 r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 fixedFormat decPointCount |
    self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
    fBase := base asFloat.
    significand := self significandAsInteger.
    roundingIncludesLimits := significand even.
    exp := (self exponent - 52) max: MinValLogBase2.
    baseExpEstimate := (self exponent * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
    exp >= 0
      ifTrue:
        [be := 1 << exp.
        significand ~= 16r10000000000000
          ifTrue:
            [r := significand * be * 2.
            s := 2.
            mPlus := be.
            mMinus := be]
          ifFalse:
            [be1 := be * 2.
            r := significand * be1 * 2.
            s := 4.
            mPlus := be1.
            mMinus := be]]
      ifFalse:
        [(exp = MinValLogBase2) | (significand ~= 16r10000000000000)
          ifTrue:
            [r := significand * 2.
            s := (1 << (exp negated)) * 2.
            mPlus := 1.
            mMinus := 1]
          ifFalse:
            [r := significand * 4.
            s := (1 << (exp negated + 1)) * 2.
            mPlus := 2.
            mMinus := 1]].
    baseExpEstimate >= 0
      ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
      ifFalse:
        [scale := base raisedToInteger: baseExpEstimate negated.
        r := r * scale.
        mPlus := mPlus * scale.
        mMinus := mMinus * scale].
    (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s))
      ifTrue: [baseExpEstimate := baseExpEstimate + 1]
      ifFalse:
        [r := r * base.
        mPlus := mPlus * base.
        mMinus := mMinus * base].
    (fixedFormat := baseExpEstimate between: -3 and: 6)
      ifTrue:
        [decPointCount := baseExpEstimate.
        baseExpEstimate <= 0
          ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
      ifFalse:
        [decPointCount := 1].
    [d := r // s.
    r := r \\ s.
    (tc1 := (r < mMinus) | (roundingIncludesLimits & (r = mMinus))) |
    (tc2 := (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s)))] whileFalse:
      [aStream nextPut: (Character digitValue: d).
      r := r * base.
      mPlus := mPlus * base.
      mMinus := mMinus * base.
      decPointCount := decPointCount - 1.
      decPointCount = 0 ifTrue: [aStream nextPut: $.]].
    tc2 ifTrue:
      [tc1 not | (tc1 & (r*2 >= s)) ifTrue: [d := d + 1]].
    aStream nextPut: (Character digitValue: d).
    decPointCount > 0
      ifTrue:
      [decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
      aStream nextPutAll: '.0'].
    fixedFormat ifFalse:
      [aStream nextPut: $e.
      aStream nextPutAll: (baseExpEstimate - 1) printString].

- absPrintOn: aStream base: base
    "Print my value on a stream in the given base.  Assumes that my value is strictly
    positive; negative numbers, zero, and NaNs have already been handled elsewhere.
    Based upon the algorithm outlined in:
    Robert G. Burger and R. Kent Dybvig
    Printing Floating Point Numbers Quickly and Accurately
    ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
    June 1996.
    This version performs all calculations with Floats instead of LargeIntegers, and loses
    about 3 lsbs of accuracy compared to an exact conversion."

    | significantBits fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
    self isInfinite ifTrue: [aStream nextPutAll: 'Infinity'. ^ self].
    significantBits := 50.  "approximately 3 lsb's of accuracy loss during conversion"
    fBase := base asFloat.
    exp := self exponent.
    baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
    exp >= 0
      ifTrue:
        [r := self.
        s := 1.0.
        mPlus := 1.0 timesTwoPower: exp - significantBits.
        mMinus := self significand ~= 1.0 ifTrue: [mPlus] ifFalse: [mPlus / 2.0]]
      ifFalse:
        [r := self timesTwoPower: significantBits.
        s := 1.0 timesTwoPower:  significantBits.
        mMinus := 1.0 timesTwoPower: (exp max: -1024).
        mPlus :=
          (exp = MinValLogBase2) | (self significand ~= 1.0)
            ifTrue: [mMinus]
            ifFalse: [mMinus * 2.0]].
    baseExpEstimate >= 0
      ifTrue:
        [s := s * (fBase raisedToInteger: baseExpEstimate).
        exp = 1023
          ifTrue:   "scale down to prevent overflow to Infinity during conversion"
            [r := r / fBase.
            s := s / fBase.
            mPlus := mPlus / fBase.
            mMinus := mMinus / fBase]]
      ifFalse:
        [exp < -1023
          ifTrue:   "scale up to prevent denorm reciprocals overflowing to Infinity"
            [d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
            scale := fBase raisedToInteger: d.
            r := r * scale.
            mPlus := mPlus * scale.
            mMinus := mMinus * scale.
            scale := fBase raisedToInteger: (baseExpEstimate + d) negated]
          ifFalse:
          [scale := fBase raisedToInteger: baseExpEstimate negated].
        s := s / scale].
    (r + mPlus >= s)
      ifTrue: [baseExpEstimate := baseExpEstimate + 1]
      ifFalse:
        [s := s / fBase].
    (fixedFormat := baseExpEstimate between: -3 and: 6)
      ifTrue:
        [decPointCount := baseExpEstimate.
        baseExpEstimate <= 0
          ifTrue: [aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate)]]
      ifFalse:
        [decPointCount := 1].
    [d := (r / s) truncated.
    r :=r - (d * s).
    (tc1 := r <= mMinus) | (tc2 := r + mPlus >= s)] whileFalse:
      [aStream nextPut: (Character digitValue: d).
      r := r * fBase.
      mPlus := mPlus * fBase.
      mMinus := mMinus * fBase.
      decPointCount := decPointCount - 1.
      decPointCount = 0 ifTrue: [aStream nextPut: $.]].
    tc2 ifTrue:
      [tc1 not | (tc1 & (r*2.0 >= s)) ifTrue: [d := d + 1]].
    aStream nextPut: (Character digitValue: d).
    decPointCount > 0
      ifTrue:
      [decPointCount - 1 to: 1 by: -1 do: [:i | aStream nextPut: $0].
      aStream nextPutAll: '.0'].
    fixedFormat ifFalse:
      [aStream nextPut: $e.
      aStream nextPutAll: (baseExpEstimate - 1) printString].

- hex  "If ya really want to know..."
    | word nibble |
    ^ String streamContents:
      [:strm |
      1 to: 2 do:
        [:i | word := self at: i.
        1 to: 8 do:
          [:s | nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
          strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]].

- printOn: aStream base: base
    "Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:"

    self isNaN ifTrue: [aStream nextPutAll: 'NaN'. ^ self]. "check for NaN before sign"
    self > 0.0
      ifTrue: [self absPrintOn: aStream base: base]
      ifFalse:
        [self sign = -1
          ifTrue: [aStream nextPutAll: '-'].
        self = 0.0
          ifTrue: [aStream nextPutAll: '0.0'. ^ self]
          ifFalse: [self negated absPrintOn: aStream base: base]].


- absPrintOn: aStream base: base digitCount: digitCount
    "Print me in the given base, using digitCount significant figures."

    | fuzz x exp q fBase scale logScale |
    self isInf ifTrue: [^ aStream nextPutAll: 'Inf'].
    fBase := base asFloat.
    "x is myself normalized to [1.0, fBase), exp is my exponent"
    exp :=
      self < 1.0
        ifTrue: [self reciprocalFloorLog: fBase]
        ifFalse: [self floorLog: fBase].
    scale := 1.0.
    logScale := 0.
    [(x := fBase raisedTo: (exp + logScale)) = 0]
      whileTrue:
        [scale := scale * fBase.
        logScale := logScale + 1].
    x := self * scale / x.
    fuzz := fBase raisedTo: 1 - digitCount.
    "round the last digit to be printed"
    x := 0.5 * fuzz + x.
    x >= fBase
      ifTrue:
        ["check if rounding has unnormalized x"
        x := x / fBase.
        exp := exp + 1].
    (exp < 6 and: [exp > -4])
      ifTrue:
        ["decimal notation"
        q := 0.
        exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000' at: i)]]]
      ifFalse:
        ["scientific notation"
        q := exp.
        exp := 0].
    [x >= fuzz]
      whileTrue:
        ["use fuzz to track significance"
        i := x asInteger.
        aStream nextPut: (Character digitValue: i).
        x := x - i asFloat * fBase.
        fuzz := fuzz * fBase.
        exp := exp - 1.
        exp = -1 ifTrue: [aStream nextPut: $.]].
    [exp >= -1]
      whileTrue:
        [aStream nextPut: $0.
        exp := exp - 1.
        exp = -1 ifTrue: [aStream nextPut: $.]].
    q ~= 0
      ifTrue:
        [aStream nextPut: $e.
        q printOn: aStream].

- deprecatedAbsPrintOn: aStream base: base digitCount: digitCount
    "Print me in the given base, using digitCount significant figures.
    This version has been deprecated by the newer version, but is kept here for comparison"

    | fuzz x exp q fBase scale logScale |
    self isInfinite ifTrue: [^ aStream nextPutAll: 'Inf'].
    fBase := base asFloat.
    "x is myself normalized to [1.0, fBase), exp is my exponent"
    exp :=
      self < 1.0
        ifTrue: [self reciprocalFloorLog: fBase]
        ifFalse: [self floorLog: fBase].
    scale := 1.0.
    logScale := 0.
    [(x := fBase raisedTo: (exp + logScale)) = 0]
      whileTrue:
        [scale := scale * fBase.
        logScale := logScale + 1].
    x := self * scale / x.
    fuzz := fBase raisedTo: 1 - digitCount.
    "round the last digit to be printed"
    x := 0.5 * fuzz + x.
    x >= fBase
      ifTrue:
        ["check if rounding has unnormalized x"
        x := x / fBase.
        exp := exp + 1].
    (exp < 6 and: [exp > -4])
      ifTrue:
        ["decimal notation"
        q := 0.
        exp < 0 ifTrue: [1 to: 1 - exp do: [:i | aStream nextPut: ('0.0000' at: i)]]]
      ifFalse:
        ["scientific notation"
        q := exp.
        exp := 0].
    [x >= fuzz]
      whileTrue:
        ["use fuzz to track significance"
        i := x asInteger.
        aStream nextPut: (Character digitValue: i).
        x := x - i asFloat * fBase.
        fuzz := fuzz * fBase.
        exp := exp - 1.
        exp = -1 ifTrue: [aStream nextPut: $.]].
    [exp >= -1]
      whileTrue:
        [aStream nextPut: $0.
        exp := exp - 1.
        exp = -1 ifTrue: [aStream nextPut: $.]].
    q ~= 0
      ifTrue:
        [aStream nextPut: $e.
        q printOn: aStream].

"Float methodsFor: 'private'"

- testPrintOn: aStream base: base
    "Print me in the given base,"

    | fBase significand exp baseExpEstimate be be1 r s mPlus mMinus scale roundingIncludesLimits d tc1 tc2 |
    self isInf ifTrue: [^ aStream nextPutAll: 'Inf'].
    fBase := base asFloat.
    significand := self significandAsInteger.
    roundingIncludesLimits := significand even.
    exp := (self exponent - 52) max: MinValLogBase2.
    baseExpEstimate := (exp + 52 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
    exp >= 0
      ifTrue:
        [be := 1 << exp.
        significand ~= 16r10000000000000
          ifTrue:
            [r := significand * be * 2.
            s := 2.
            mPlus := be.
            mMinus := be]
          ifFalse:
            [be1 := be * 2.
            r := significand * be1 * 2.
            s := 4.
            mPlus := be1.
            mMinus := be]]
      ifFalse:
        [(exp = MinValLogBase2) | (significand ~= 16r10000000000000)
          ifTrue:
            [r := significand * 2.
            s := (1 << (exp negated)) * 2.
            mPlus := 1.
            mMinus := 1]
          ifFalse:
            [r := significand * 4.
            s := (1 << (exp negated + 1)) * 2.
            mPlus := 2.
            mMinus := 1]].
    baseExpEstimate >= 0
      ifTrue: [s := s * (base raisedToInteger: baseExpEstimate)]
      ifFalse:
        [scale := base raisedToInteger: baseExpEstimate negated.
        r := r * scale.
        mPlus := mPlus * scale.
        mMinus := mMinus * scale].
    (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s))
      ifTrue: [baseExpEstimate := baseExpEstimate + 1]
      ifFalse:
        [r := r * base.
        mPlus := mPlus * base.
        mMinus := mMinus * base].

    [d := r // s.
    r :=_ r \\ s.
    (tc1 := (r < mMinus) | (roundingIncludesLimits & (r = mMinus))) |
    (tc2 := (r + mPlus > s) | (roundingIncludesLimits & (r + mPlus = s)))] whileFalse:
      [aStream nextPut: (Character digitValue: d).
      r := r * base.
      mPlus := mPlus * base.
      mMinus := mMinus * base].
    tc2 ifTrue:
      [tc1 not | (tc1 & (r*2 >= s)) ifTrue: [d := d + 1]].
    aStream nextPut: (Character digitValue: d).
    aStream nextPut: $e.
    aStream nextPutAll: baseExpEstimate printString.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Float class methodsFor: 'class initialization'"

+ initialize
    "Float initialize"
    "Constants from Computer Approximations, pp. 182-183:
      Pi = 3.14159265358979323846264338327950288
      Pi/2 = 1.57079632679489661923132169163975144
      Pi*2 = 6.28318530717958647692528676655900576
      Pi/180 = 0.01745329251994329576923690768488612
      2.0 ln = 0.69314718055994530941723212145817657
      2.0 sqrt = 1.41421356237309504880168872420969808"

    Pi := 3.14159265358979323846264338327950288.
    Halfpi := Pi / 2.0.
    Twopi := Pi * 2.0.
    RadiansPerDegree := Pi / 180.0.

    Ln2 := 0.69314718055994530941723212145817657.
    Ln10 := 10.0 ln.
    Sqrt2 := 1.41421356237309504880168872420969808.
    E := 2.718281828459045235360287471353.

    Epsilon := 0.000000000001.  "Defines precision of mathematical functions"

    MaxVal := 1.7976931348623159e308.
    MaxValLn := 709.782712893384.
    MinValLogBase2 := -1074.

    Infinity := MaxVal * MaxVal.
    NegativeInfinity := 0.0 - Infinity.
    NaN := Infinity - Infinity.
    NegativeZero := 1.0 / Infinity negated.

+ readFrom: aStream
    "Answer a new Float as described on the stream, aStream."

    ^ (super readFrom: aStream) asFloat.


"Float class methodsFor: 'constants'"

+ e
    "Answer the constant, E."
    ^ E.

+ infinity
    "Answer the value used to represent an infinite magnitude"
    ^ Infinity.

+ nan
	"Answer the canonical value used to represent Not-A-Number"
	^ NaN.

+ negativeZero
	^ NegativeZero.

+ pi
	"Answer the constant, Pi."
	^ Pi.
