"This is a placeholder to test bootstrapping"

Object subclass: #Behavior
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Behavior commentStamp: 'al 12/8/2005 20:44' prior: 0!
My instances describe the behavior of other objects. I provide the minimum state necessary for compiling methods, and creating and running instances. Most objects are created as instances of the more fully supported subclass, Class, but I am a good starting point for providing instance-specific behavior (as in Metaclass).!

!Behavior methodsFor: 'instance creation' stamp: 'Noury Bouraqadi 8/23/2003 14:51'!
new
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	^ self basicNew initialize
! !
