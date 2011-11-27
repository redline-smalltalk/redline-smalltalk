" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Assert

+ assert: aBlock withMessage: aString
    aBlock value ifFalse: [ Object error: aString ].
