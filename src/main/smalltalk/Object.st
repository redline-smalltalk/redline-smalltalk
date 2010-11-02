
ProtoObject subclass: #Object
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

!Object commentStamp: '<historical>' prior: 0!

Object is the root class for almost all of the other classes in the class hierarchy. The exceptions are ProtoObject (the superclass
of Object) and its subclasses.

Class Object provides default behavior common to all normal objects, such as access, copying, comparison, error handling, message
sending, and reflection. Also utility messages that all objects should respond to are defined here.

Object has no instance variables, nor should any be added. This is due to several classes of objects that inherit from Object that
have special implementations (SmallInteger and UndefinedObject for example) or the VM knows about and depends on the structure and
layout of certain standard classes.

Because Object is the root of the inheritance tree, methods are often defined in Object to give all objects special behaviors needed
by certain subsystems or applications, or to respond to certain general test messages such as isMorph.!

!Object methodsFor: 'initialize-release' stamp: 'jcl 19/11/2010 09:17'!
initialize
	"Subclasses should redefine this method to perform initializations on instance creation"
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Object class
	instanceVariableNames: ''!

!Object class methodsFor: 'as yet unclassified' stamp: 'jcl 19/11/2010 09:17'!
classPlaceholder
	"Answer nil."
	^ nil
! !
