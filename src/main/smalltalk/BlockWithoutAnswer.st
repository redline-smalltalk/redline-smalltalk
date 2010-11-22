
Object subclass: #BlockWithoutAnswer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Block'!

!BlockWithoutAnswer commentStamp: '<historical>' prior: 0!
I represent a block of code that has no ^ answer statement.!

!BlockWithoutAnswer methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
placeholder
	"Answer nil."
	^ nil
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

BlockWithoutAnswer class
	instanceVariableNames: ''!

!BlockWithoutAnswer class methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
classPlaceholder
	"Answer nil."
	^ nil
! !
