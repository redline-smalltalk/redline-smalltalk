
Object subclass: #Boolean
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Boolean commentStamp: '<historical>' prior: 0!
Boolean is an abstract class defining the protocol for logic testing operations and conditional control structures for the logical
values represented by the instances of its subclasses True and False.

Boolean redefines #new so no instances of Boolean can be created. It also redefines several messages in the 'copying' protocol to
ensure that only one instance of each of its subclasses True (the global true, logical assertion) and False (the global false,
logical negation) ever exist in the system.!

!Boolean methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
placeholder
	"Answer nil."
	^ nil
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Boolean class
	instanceVariableNames: ''!

!Boolean class methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
classPlaceholder
	"Answer nil."
	^ nil
! !
