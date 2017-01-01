"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Number subclass: #Integer.

"category: factorization and divisibility"

- factorial
    "Answer the factorial of the receiver. The receiver must not be less than 0."
    self notImplementedYet.

- gcd: anInteger
    "Answer the greatest common divisor of the receiver and the argument, anInteger."
    self notImplementedYet.

- lcm: anInteger
    "Answer the least common multiple of the receiver and the argument, anInteger."
    self notImplementedYet.

"category: bit manipulation"

- allMask: anInteger
    "Treat the argument anInteger as a bit mask. Answer whether all of the bits that are 1 in anInteger are 1 in the
     receiver."
    self notImplementedYet.

- anyMask: anInteger
    "Treat the argument anInteger as a bit mask. Answer whether any of the bits that are 1 in anInteger are 1 in the
     receiver."
    self notImplementedYet.

- noMask: anInteger
    "Treat the argument anlnteger as a bit mask. Answer whether none of the bits that are 1 in anInteger are 1 in the
     receiver."
    self notImplementedYet.

- bitAnd: anInteger
    "Answer an Integer whose bits are the logical and of the receiver's bits and those of the argument anInteger."
    self notImplementedYet.

- bitOr: anInteger
    "Answer an Integer whose bits are the logical or of the receiver's bits and those of the argument anInteger."
    self notImplementedYet.

- bitXor: anInteger
    "Answer an Integer whose bits are the logical xor of the receiver's bits and those of the argument anInteger."
    self notImplementedYet.

- bitAt: index
    "Answer the bit (0 or 1) at position index of the receiver."
    self notImplementedYet.

- bitInvert
    "Answer an Integer whose bits are the complement of the receiver."
    self notImplementedYet.

- highBit
    "Answer the index of the high order bit of the binary representation of the receiver."
    self notImplementedYet.

- bitShift: anInteger
    "Answer an Integer whose value (in two's-complement representation) is the receiver's value (in two's-complement
     representation) shifted left by the number of bits indicated by the argument, anInteger. Negative arguments shift
     right. Zeros are shifted in from the right in left shifts. The sign bit is extended in right shifts."
    self notImplementedYet.
