" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Magnitude
"comparing"
- < aMagnitude
    self subclassResponsibility.

- <= aMagnitude
    self subclassResponsibility.

- = aMagnitude
    self subclassResponsibility.

- \> aMagnitude
    self subclassResponsibility.

- >= aMagnitude
    self subclassResponsibility.

- between: min and: max
    ^ self >= min and: [self <= max].

"testing"
- min: aMagnitude
    self < aMagnitude ifTrue: [ ^ self ] ifFalse: [ ^ aMagnitude ].

- max: aMagnitude
    self \> aMagnitude ifTrue: [ ^ self ] ifFalse: [ ^ aMagnitude ].