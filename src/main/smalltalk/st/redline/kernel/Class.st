"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ClassDescription subclass: #Class.

"category: subclass creation"

- subclass: name
    "Create a new subclass of the receiver. This is a convenience message that delegates to the default."
    ^ self subclass: name instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' classInstanceVariableNames: ''.

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts classInstanceVariableNames: classInstVarNames
    "Create a new subclass of the receiver. Primitives >127 added by Redline."
    <primitive: 128>
