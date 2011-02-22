
"im a comment"

Object subclass: #TestMethod
	instanceVariableNames: 'instVars'
	classVariableNames: 'ClassVars'
	poolDictionaries: 'Pool'
	category: 'Test'!

!Object methodsFor: '*Polymorph-EventEnhancements' stamp: 'gvc 10/25/2006 18:18'!

example
  | var |
  ^ self + 'bar'
! !

methodWithPrimitive
  <primitive: 1>
  ^ self
! !

