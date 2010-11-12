
Behavior subclass: #ClassDescription
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!ClassDescription commentStamp: '<historical>' prior: 0!
I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	The maintenance of a ChangeSet, and logging changes on a file
	Most of the mechanism for fileOut.

I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.

The slots 'organization' and 'methodDict' should ONLY be accessed by message in order for things to work during
ImageSegment>>discoverActiveClasses (q.v.).!

!ClassDescription methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
placeholder
	"Answer nil."
	^ nil
! !
