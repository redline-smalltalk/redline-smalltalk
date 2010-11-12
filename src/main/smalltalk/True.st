
Boolean subclass: #True
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!True commentStamp: '<historical>' prior: 0!
True defines the behavior of its single instance, true -- logical assertion. Notice how the truth-value checks become direct message
sends, without the need for explicit testing.

Be aware however that most of these methods are not sent as real messages in normal use. Most are inline coded by the compiler as
test and jump bytecodes - avoiding the overhead of the full message sends. So simply redefining these methods here will have no
effect.!

!True methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
placeholder
	"Answer nil."
	^ nil
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

True class
	instanceVariableNames: ''!

!True class methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
classPlaceholder
	"Answer nil."
	^ nil
! !
