" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Magnitude
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