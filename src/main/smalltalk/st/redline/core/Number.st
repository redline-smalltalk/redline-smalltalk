" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Magnitude < #Number.

"arithmetic"

- * aNumber 
    "Answer the result of multiplying the receiver by aNumber."

    self subclassResponsibility

- + aNumber 
    "Answer the sum of the receiver and aNumber."

    self subclassResponsibility
  
- - aNumber 
    "Answer the difference between the receiver and aNumber."

    self subclassResponsibility
  
- / aNumber 
    "Answer the result of dividing the receiver by aNumber."

    self subclassResponsibility
          
"intervals"

- to: stop
    ^ Interval from: self to: stop by: 1.

- to: stop do: aBlock
    | nextValue |
    nextValue := self.
    [ nextValue <= stop ]
      whileTrue: [
        aBlock value: nextValue.
        nextValue := nextValue + 1
      ].

"testing"

- negative
    "Answer whether the receiver is mathematically negative."

    ^ self < 0

"truncation"

- floor
    "Answer the integer nearest the receiver toward negative infinity."

    self subclassResponsibility.

- truncated
    "Answer an integer nearest the receiver toward zero."

    self subclassResponsibility.
