" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Magnitude < #Character
	instanceVariableNames: 'value';
	classVariableNames: 'CharacterTable DigitValues';
	classInstanceVariableNames: '';
	poolDictionaries: '';
	category: ''.

+ initialize
    CharacterTable ifNil: [
        CharacterTable := Array new: 256.
        1 to: 256 do: [:i | CharacterTable at: i put: (self basicNew setValue: i - 1)]].
    self initializeDigitValues.

+ initializeDigitValues
    DigitValues := Array new: 256 withAll: -1.

    0 to: 9 do: [:i | DigitValues at: 48 + i + 1 put: i].

    10 to: 35 do: [:i | DigitValues at: 55 + i + 1 put: i].

    10 to: 35 do: [:i | DigitValues at: 87 + i + 1 put: i].