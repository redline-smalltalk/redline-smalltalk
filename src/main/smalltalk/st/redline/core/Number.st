"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Magnitude subclass: #Number.

"category: arithmetic"

- + aNumber
    "Answer the sum of the receiver and the argument, aNumber."
    self notImplementedYet.

- - aNumber
    "Answer the difference between the receiver and the argument, aNumber."
    self notImplementedYet.

- * aNumber
    "Answer the result of multiplying the receiver by the argument, aNumber."
    self notImplementedYet.

- / aNumber
    "Answer the result of dividing the receiver by the argument, aNumber. Note that since as much precision as possible
     is retained, if the division is not exact, the result will be an instance of Fraction."
    self notImplementedYet.

- // aNumber
    "Answer the integer quotient defined by division with truncation toward negative infinity."
    self notImplementedYet.

- \\ aNumber
    "Answer the integer remainder defined by division with truncation toward negative infinity. This is the modulo
     operation."
    self notImplementedYet.

- abs
    "Answer a Number that is the absolute value (positive magnitude) of the receiver."
    self notImplementedYet.

- negated
    "Answer a Number that is the negation of the receiver."
    self notImplementedYet.

- quo: aNumber
    "Answer the integer quotient defined by division with truncation toward zero."
    self notImplementedYet.

- rem: aNumber
    "Answer the integer remainder defined by division with truncation toward zero."
    self notImplementedYet.

- reciprocal
    "Answer 1 divided by the receiver. Report an error to the user if the receiver is 0."
    self notImplementedYet.

"category: mathematical functions"

- exp
    "Answer a floating point number that is the exponential of the receiver."
    self notImplementedYet.

- ln
    "Answer the natural log of the receiver."
    self notImplementedYet.

- log: aNumber
    "Answer the log base aNumber of the receiver."
    self notImplementedYet.

- floorLog: radix
    "Answer the floor of the log base radix of the receiver, where the floor is the integer nearest the receiver toward
     negative infinity."
    self notImplementedYet.

- raisedTo: aNumber
    "Answer the receiver raised to the power of the argument, aNumber."
    self notImplementedYet.

- raisedToInteger: anInteger
    "Answer the receiver raised to the power of the argument, anInteger, where the argument must be a kind of Integer."
    self notImplementedYet.

- sqrt
    "Answer a floating point number that is the positive square root of the receiver."
    self notImplementedYet.

- squared
    "Answer the receiver multiplied by itself."
    self notImplementedYet.

"category: testing"

- even
    "Answer whether the receiver is an even number."
    self notImplementedYet.

- odd
    "Answer whether the receiver is an odd number."
    self notImplementedYet.

- negative
    "Answer whether the receiver is less than 0."
    self notImplementedYet.

- positive
    "Answer whether the receiver is greater than or equal to 0."
    self notImplementedYet.

- strictlyPositive
    "Answer whether the receiver is greater than 0."
    self notImplementedYet.

- sign
    "Answer 1 if the receiver is greater than 0, answer -1 if less than 0, else answer 0."
    self notImplementedYet.

"category: truncation and round off"

- ceiling
    "Answer the integer nearest the receiver toward positive infinity."
    self notImplementedYet.

- floor
    "Answer the integer nearest the receiver toward negative infinity."
    self notImplementedYet.

- truncated
    "Answer the integer nearest the receiver toward zero."
    self notImplementedYet.

- truncateTo: aNumber
    "Answer the next multiple of the argument, aNumber, that is nearest the receiver toward zero."
    self notImplementedYet.

- rounded
    "Answer the integer nearest the receiver."
    self notImplementedYet.

- roundTo: aNumber
    "Answer the multiple of the argument, aNumber, that is nearest the receiver."
    self notImplementedYet.

"category: converting"

- degreesToRadians
    "The receiver is assumed to represent degrees. Answer the conversion to radians."
    self notImplementedYet.

- radiansToDegrees
    "The receiver is assumed to represent radians. Answer the conversion to degrees."
    self notImplementedYet.

"category: coercing"

- coerce: aNumber
    "Answer a number representing the argument, aNumber, that is the same kind of Number as the receiver. This method
     must be defined by all subclasses of Number."
    self notImplementedYet.

- generality
    "Answer the number representing the ordering of the receiver in the generality hierarchy."
    self notImplementedYet.

- retry: aSymbol coercing: aNumber
    "An arithmetic operation denoted by the symbol, aSymbol, could not be performed with the receiver and the argument,
     aNumber, as the operands because of the difference in representation. Coerce either the receiver or the argument,
     depending on which has the lower generality, and then try the arithmetic operation again. If the symbol is the
     equals sign, answer false if the argument is not a Number. If the generalities are the same, then retry:coercing:
     should not have been sent, so report an error to the user."
    self notImplementedYet.

"category: intervals"

- to: stop
    "Answer an Interval from the receiver up to the argument, stop, with each next element computed by incrementing the
     previous one by 1."
    self notImplementedYet.

- to: stop by: step
    "Answer an Interval from the receiver up to the argument, stop, with each next element computed by incrementing the
     previous one by step."
    self notImplementedYet.

- to: stop do: aBlock
    "Create an Interval from the receiver up to the argument, stop, incrementing by 1. Evaluate the argument, aBlock,
     for each element of the Interval."
    self notImplementedYet.

- to: stop by: step do: aBlock
    "Create an Interval from the receiver up to the argument, stop, incrementing by step. Evaluate the argument, aBlock,
     for each element of the Interval."
    self notImplementedYet.
