
Object subclass: #ExceptionSet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!

!ExceptionSet commentStamp: '<historical>' prior: 0!
An ExceptionSet is a grouping of exception handlers which acts as a single handler.  Within the group, the most recently added
handler will be the last handler found during a handler search (in the case where more than one handler in the group is capable of
handling a given exception). !

!ExceptionSet methodsFor: 'exceptionselector' stamp: 'tfei 6/4/1999 18:37'!
, anException
	"Return an exception set that contains the receiver and the argument exception. This is commonly used to specify a set of
	exception selectors for an exception handler."

	self add: anException.
	^ self
! !