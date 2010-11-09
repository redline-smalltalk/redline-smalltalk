
Object subclass: #Exception
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Exceptions-Kernel'!

!Exception commentStamp: '<historical>' prior: 0!
This is the main class used to implement the exception handling system (EHS).  It plays two distinct roles:  that of the exception,
and that of the exception handler.  More specifically, it implements the bulk of the protocols laid out in the ANSI specification
- those protocol names are reflected in the message categories.

Exception is an abstract class.  Instances should neither be created nor trapped.  In most cases, subclasses should inherit from
Error or Notification rather than directly from Exception.

In implementing this EHS, The Fourth Estate Inc. incorporated some ideas and code from Craig Latta's EHS.  His insights were crucial
in allowing us to implement BlockContext>>valueUninterruptably (and by extension, #ensure: and #ifCurtailed:), and we imported the
following methods with little or no modification:

ContextPart>>terminateTo:
ContextPart>>terminate
MethodContext>>receiver:
MethodContext>>answer:

Thanks, Craig!

!Exception methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
placeholder
	"Answer nil."
	^ nil
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Exception class
	instanceVariableNames: ''!

!Exception class methodsFor: 'exceptioninstantiator' stamp: 'ajh 9/30/2001 21:54'!
signal
	"Signal the occurrence of an exceptional condition."
	^ self new signal
! !
