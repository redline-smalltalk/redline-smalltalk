"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

PrimObject subclass: #Object.

+ newFrom: aSimilarObject
	| myInstVars similarInstVars any inst good |
	myInstVars := self allInstVarNames.
	similarInstVars := aSimilarObject class allInstVarNames.
	inst := self new.
	myInstVars
		doWithIndex:
			[:each :index |
			good := similarInstVars indexOf: each.
			good > 0
				ifTrue:
					[inst instVarAt: index put: (aSimilarObject instVarAt: good).
					any := true]].
	any == nil ifTrue: [self subclassResponsibility].
	^ inst.
