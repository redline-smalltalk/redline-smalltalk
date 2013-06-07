"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Magnitude subclass: #Character.

+ value: anInteger
    "Answer an instance of Character whose value is the argument, anInteger. The value is associated with an element of
     the ASCII character set. For example, Character value: 65 is a capital 'A'."
    self notImplementedYet.

+ digitValue: anInteger
    "Answer an instance of Character whose digit value is the argument, anInteger. For example, answer $9 if the
     argument is 9; answer $0 for 0; answer $A for 10, and $Z for 35. This method is useful in parsing numbers into
     strings. Typically, only Characters up to $F are useful (for base 16 numbers)."
    self notImplementedYet.

"category: accessing"

- asciiValue
    "Answer the number corresponding to the ASCII encoding for the receiver."
    self notImplementedYet.

- digitValue
    "Answer the number corresponding to the numerical radix represented by the receiver (see the instance creation
     message digitValue: for the correspondences)."
    self notImplementedYet.

"category: testing"

- isAlphaNumeric
    "Answer true if the receiver is a letter or a digit."
    self notImplementedYet.

- isDigit
    "Answer whether the receiver is a digit."
    self notImplementedYet.

- isLetter
    "Answer whether the receiver is a letter."
    self notImplementedYet.

- isLowercase
    "Answer whether the receiver is a lowercase letter."
    self notImplementedYet.

- isUppercase
    "Answer whether the receiver is an uppercase letter."
    self notImplementedYet.

- isSeparator
    "Answer whether the receiver is one of the separator characters in the expression syntax: space, cr, tab, line feed,
     or form feed."
    self notImplementedYet.

- isVowel
    "Answer whether the receiver is one of the vowels: a, e, i, o, or u, in upper or lowercase."
    self notImplementedYet.
