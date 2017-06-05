"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a collection of links, which are containers for other objects. Using the message sequence
 addFirst:/removeLast causes the receiver to behave as a stack; using addLast:/removeFirst causes the receiver to
 behave as a queue."

SequenceableCollection subclass: #LinkedList
	                   instanceVariableNames: 'firstLink lastLink '
	                   category: 'Collections-Sequenceable'.

"LinkedList methodsFor: 'accessing'"

- first
	"Answer the first link. Create an error notification if the receiver is
	empty."

	self emptyCheck.
	^firstLink.

- last
	"Answer the last link. Create an error notification if the receiver is
	empty."

	self emptyCheck.
	^lastLink.

- size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^tally.


"LinkedList methodsFor: 'testing'"

- isEmpty

	^firstLink == nil.


"LinkedList methodsFor: 'adding'"

- add: aLink
	"Add aLink to the end of the receiver's list. Answer aLink."

	^self addLast: aLink.

- addFirst: aLink
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink.

- addLast: aLink
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty
		ifTrue: [firstLink := aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink.


"LinkedList methodsFor: 'removing'"

- remove: aLink ifAbsent: aBlock
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink := aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := nil]]
		ifFalse: [tempLink := firstLink.
				[tempLink == nil ifTrue: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink := tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := tempLink]].
	aLink nextLink: nil.
	^aLink.

- removeFirst
	"Remove the first element and answer it. If the receiver is empty, create
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [firstLink := oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink.

- removeLast
	"Remove the receiver's last element and answer it. If the receiver is
	empty, create an error notification."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [aLink := firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink := aLink nextLink].
				 aLink nextLink: nil.
				 lastLink := aLink].
	oldLink nextLink: nil.
	^oldLink.


"LinkedList methodsFor: 'enumerating'"

- do: aBlock

	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink := aLink nextLink].
