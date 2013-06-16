"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ArrayedCollection subclass: #String.

"category: comparing"

- < aString
    "Answer whether the receiver collates before the argument, aString. The collation sequence is ASCII with case
     differences ignored."
    self notImplementedYet.

- <= aString
    "Answer whether the receiver collates before the argument, aString, or is the same as aString. The collation
     sequence is ASCII with case differences ignored."
    self notImplementedYet.

- > aString
    "Answer whether the receiver collates after the argument, aString. The collation sequence is ASCII with case
     differences ignored."
    self notImplementedYet.

- >= aString
    "Answer whether the receiver collates after the argument, aString, or is the same as aString. The collation
     sequence is ASCII with case differences ignored."
    self notImplementedYet.

- match: aString
    "Treat the receiver as a pattern that can contain characters # and *. Answer whether the argument, aString, matches
     the pattern in the receiver. Matching ignores upper/lower case differences. Where the receiver contains the \
     character #:, aString may contain any single character. Where the receiver contains ,, aString may contain any
     sequence of characters, including no characters."
    self notImplementedYet.

- sameAs: aString
    "Answer whether the receiver collates precisely with the argument, aString. The collation sequence is ASCII with
     case differences ignored."
    self notImplementedYet.

"category: converting"

- asLowercase
    "Answer a String made up from the receiver whose characters are all lowercase."
    self notImplementedYet.

- asUppercase
    "Answer a String made up from the receiver whose characters are all uppercase."
    self notImplementedYet.

- asSymbol
    "Answer the unique Symbol whose characters are the characters of the receiver."
    self notImplementedYet.
