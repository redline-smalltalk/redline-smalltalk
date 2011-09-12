" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

ClassDescription < #Class

- subclass: aSymbol instanceVariableNames: instVars classVariableNames: classVars classInstanceVariableNames: classInstVars poolDictionaries: poolDicts category: cat
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver)."
	self < aSymbol;
		instanceVariableNames: instVars;
		classVariableNames: classVars;
		classInstanceVariableNames: classInstVars;
		poolDictionaries: poolDicts;
		category: cat.

