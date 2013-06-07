"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ClassDescription subclass: #Class.

"category: accessing instances and variables"

- addClassVarName: aString
    "Add the argument, aString, as a class variable of the receiver. The first character of aString must be capitalized;
     aString can not already be a class variable name."
    self notImplementedYet.

- removeClassVarName: aString
    "Remove the receiver's class variable whose name is the argument, aString. Report an error if it is not a class
     variable or if it is still being used in a method of the class."
    self notImplementedYet.

- addSharedPool: aDictionary
    "Add the argument, aDictionary, as a pool of shared variables. Report an error if the dictionary is already a shared
     pool in the receiver."
    self notImplementedYet.

- removeSharedPool: aDictionary
    "Remove the argument, aDictionary, as one of the receiver's pool dictionaries. Report an error if the dictionary is
     not one of the receiver's pools."
    self notImplementedYet.

- classPool
    "Answer the dictionary of class variables of the receiver."
    self notImplementedYet.

- initialize
    "Initialize class variables."
    self notImplementedYet.

"category: instance creation"

- subclass: name
    "Create a new subclass of the receiver. This is a convenience message that delegates to the default."
    ^ self subclass: name instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' classInstanceVariableNames: ''.

- subclass: name instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: poolDicts classInstanceVariableNames: classInstVarNames
    "Create a new subclass of the receiver. Primitives >127 added by Redline."
    <primitive: 128>
