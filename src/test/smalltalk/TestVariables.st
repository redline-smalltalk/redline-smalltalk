
"im a comment"

Object subclass: #TestVariables
	instanceVariableNames: 'instVars'
	classVariableNames: 'classVars'
	classInstanceVariableNames: 'classInstVar'
	poolDictionaries: 'Pool'
	category: 'Test'

cdef variable
	^ classVars


