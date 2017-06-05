"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a character by storing its associated ASCII code (extended to 256 codes). My instances are created uniquely,
 so that all instances of a character ($R, for example) are identical."

Magnitude subclass: #Character
          instanceVariableNames: 'value '
          classVariableNames: 'CharacterTable '
          poolDictionaries: ''
          category: 'Collections-Text'.

"Character methodsFor: 'accessing'"

- asciiValue
    "Answer the value of the receiver that represents its ascii encoding."
    ^ value.

- digitValue
    "Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0
    otherwise. This is used to parse literal numbers of radix 2-36."

    value <= $9 asciiValue
      ifTrue: [^ value - $0 asciiValue].
    value >= $A asciiValue
      ifTrue: [value <= $Z asciiValue ifTrue: [^ value - $A asciiValue + 10]].
    ^ -1.

"Character methodsFor: 'comparing'"

- < aCharacter
    "Answer true if the receiver's value < aCharacter's value."
    ^ self asciiValue < aCharacter asciiValue.

- = aCharacter
    "Primitive. Answer true if the receiver and the argument are the same
    object (have the same object pointer) and false otherwise. Optional. See
    Object documentation whatIsAPrimitive."

    JVM primitive: 110.

- > aCharacter
    "Answer true if the receiver's value > aCharacter's value."
    ^ self asciiValue > aCharacter asciiValue.

- hash
    "Hash is reimplemented because = is implemented."
    ^ value.


"Character methodsFor: 'testing'"

- isAlphaNumeric
    "Answer whether the receiver is a letter or a digit."

    ^ self isLetter or: [self isDigit].

- isDigit
	"Answer whether the receiver is a digit."

	^ value >= 48 and: [value <= 57].

- isLetter
	"Answer whether the receiver is a letter."

	^ (8r141 <= value and: [value <= 8r172])
		or: [8r101 <= value and: [value <= 8r132]].

- isLowercase
    "Answer whether the receiver is a lowercase letter.
    (The old implementation answered whether the receiver is not an uppercase letter.)"

    ^ 8r141 <= value and: [value <= 8r172].

- isSeparator
    "Answer whether the receiver is one of the separator characters--space,
    cr, tab, line feed, or form feed."

    value = 32 ifTrue: [^ true].	"space"
    value = 13 ifTrue: [^ true].	"cr"
    value = 9  ifTrue: [^ true].	"tab"
    value = 10 ifTrue: [^ true].	"line feed"
    value = 12 ifTrue: [^ true].	"form feed"
    ^ false.

- isSpecial
    "Answer whether the receiver is one of the special characters"

    ^ '+/\*~<>=@%|&?!!' includes: self.

- isUppercase
    "Answer whether the receiver is an uppercase letter.
    (The old implementation answered whether the receiver is not a lowercase letter.)"

    ^ 8r101 <= value and: [value <= 8r132].

- isVowel
    "Answer whether the receiver is one of the vowels, AEIOU, in upper or
    lower case."

    ^ 'AEIOU' includes: self asUppercase.

- tokenish
    "Answer whether the receiver is a valid token-character--letter, digit, or
    colon."

    ^ self isLetter or: [self isDigit or: [self = $:]].

"Character methodsFor: 'copying'"

- copy
    "Answer with the receiver because Characters are unique."

    ^ self.

- deepCopy
    "Answer with the receiver because Characters are unique."

    ^ self.


"Character methodsFor: 'printing'"

- hex

    ^ String with: ('0123456789ABCDEF' at: value//16+1)
        with:  ('0123456789ABCDEF' at: value\\16+1).

- isLiteral

	^ true.

- printOn: aStream

    aStream nextPut: $$.
    aStream nextPut: self.

- storeOn: aStream
    "Character literals are preceded by '$'."

    aStream nextPut: $$; nextPut: self.


"Character methodsFor: 'converting'"

asCharacter
    "Answer the receiver itself."

    ^ self.

- asInteger
    "Answer the value of the receiver."

    ^ value.

- asLowercase
    "If the receiver is uppercase, answer its matching lowercase Character."

    (8r101 <= value and: [value <= 8r132])  "self isUppercase"
      ifTrue: [^ Character value: value + 8r40]
      ifFalse: [^ self].

- asString
    | cString |
    cString := String new: 1.
    cString at: 1 put: self.
    ^ cString.

- asSymbol
    "Answer a Symbol consisting of the receiver as the only element."

    ^ Symbol internCharacter: self.

- asUppercase
    "If the receiver is lowercase, answer its matching uppercase Character."

    (8r141 <= value and: [value <= 8r172])  "self isLowercase"
      ifTrue: [^ Character value: value - 8r40]
      ifFalse: [^ self].

- to: other
    "Answer with a collection in ascii order -- $a to: $z"

    ^ (self asciiValue to: other asciiValue) collect:
          [:ascii | Character value: ascii].

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"Character class methodsFor: 'class initialization'"

+ initialize
	"Create the table of unique Characters. This code is not shown so that the
	user can not destroy the system by trying to recreate the table.".
  ^ self.

"Character class methodsFor: 'instance creation'"

+ digitValue: x
    "Answer the Character whose digit value is x. For example, answer $9 for
    x=9, $0 for x=0, $A for x=10, $Z for x=35."

    | index |
    index := x asInteger.
    ^ CharacterTable at:
      (index < 10
        ifTrue: [48 + index]
        ifFalse: [55 + index])
      + 1.

+ new
    "Creating new characters is not allowed."

    self error: 'cannot create new characters'.

+ separators
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character value: v].

+ value: anInteger
    "Answer the Character whose value is anInteger."

    ^ CharacterTable at: anInteger + 1.


"Character class methodsFor: 'accessing untypeable characters'"

+ backspace
    "Answer the Character representing a backspace."

    ^ self value: 8.

+ cr
	"Answer the Character representing a carriage return."

	^ self value: 13.

+ enter
    "Answer the Character representing enter."
    ^ self value: 3.

+ linefeed
    "Answer the Character representing a linefeed."
    ^ self value: 10.

+ newPage
    "Answer the Character representing a form feed."
    ^ self value: 12.

+ space
    "Answer the Character representing a space."
    ^ self value: 32.

+ tab
    "Answer the Character representing a tab."
    ^ self value: 9.


"Character class methodsFor: 'constants'"

+ alphabet
    ^ 'abdcefghijklmnopqrstuvwxyz'.

+ characterTable
    "Answer the class variable in which unique Characters are stored."
    ^ CharacterTable.
