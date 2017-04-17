"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"An instance of me is a simple record of a pointer to another Link. I am an abstract class; my concrete subclasses, for
 example, Process, can be stored in a LinkedList structure."

Object subclass: #Link
	   instanceVariableNames: 'nextLink '
	   category: 'Collections-Support'.

"Link methodsFor: 'accessing'"

- nextLink
	"Answer the link to which the receiver points."

	^nextLink.

- nextLink: aLink
	"Store the argument, aLink, as the link to which the receiver refers.
	Answer aLink."

	^nextLink := aLink.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Link class methodsFor: 'instance creation'"

+ nextLink: aLink
	"Answer an instance of me referring to the argument, aLink."

	^self new nextLink: aLink.
