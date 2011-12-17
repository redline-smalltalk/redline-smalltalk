" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Magnitude < #Character
  instanceVariableNames: 'value';
  classInstanceVariableNames: '';
  classVariableNames: 'CharacterTable DigitValues';
  poolDictionaries: '';
  category: ''.

+ newWithValue: anInteger
  <primitive: 217>

+ initialize
  CharacterTable ifNil: [
    CharacterTable := Array new: 256.
    1 to: 256 do: [:i |
      CharacterTable at: i put: (Character newWithValue: i)]].
  self initializeDigitValues.

+ initializeDigitValues
  DigitValues := Array new: 256.

  0 to: 9 do: [:i | DigitValues at: 48 + i + 1 put: i].

  10 to: 35 do: [:i | DigitValues at: 55 + i + 1 put: i].

  10 to: 35 do: [:i | DigitValues at: 87 + i + 1 put: i].

+ value: anInteger
  ^ CharacterTable at: anInteger.
