" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ClassDescription < #Class

- subclass: aSymbol instanceVariableNames: instVars classVariableNames: classVars classInstanceVariableNames: classInstVars poolDictionaries: poolDicts
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver)."
	self < aSymbol
		instanceVariableNames: instVars;
		classVariableNames: classVars;
		classInstanceVariableNames: classInstVars;
		poolDictionaries: poolDicts.

- subclass: aSymbol instanceVariableNames: instVars classVariableNames: classVars classInstanceVariableNames: classInstVars poolDictionaries: poolDicts category: cat
    "Answer a new subclass. Category is dropped as it is implied in namespace."
	self subclass: aSymbol instanceVariableNames: instVars classVariableNames: classVars classInstanceVariableNames: classInstVars poolDictionaries: poolDicts.

- subclass: aSymbol
    "Answer a new subclass aSymbol."
    self < aSymbol.

- instanceVariableNames: instVars
    "Add each instance variable to the receiver. TODO."

- classVariableNames: classVars
    "Add each class variable to the receiver. TODO."

- classInstanceVariableNames: instVars
    "Add each class instance variable to the receiver. TODO."

- poolDictionaries: poolDicts
    "Add each pool dictionary to the receiver. TODO."
