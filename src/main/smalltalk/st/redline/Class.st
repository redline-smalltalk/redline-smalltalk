" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ClassDescription < #Class.

- < name
    ^ self subclass: name instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' classInstanceVariableNames: ''.

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts classInstanceVariableNames: classInstVarNames
    "create a new subclass of the receiver. Nb: Primitive 128 is added by Redline (not in Blue Book)."
    <primitive: 128>

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts category: category
    "Provides compatability for other Smalltalks. Nb: we drop category."
    ^ self subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries classInstanceVariableNames: ''.

