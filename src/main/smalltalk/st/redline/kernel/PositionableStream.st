"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Stream subclass: #PositionableStream.

"category: instance creation."

- on: aCollection
    "Answer an instance of a kind of PositionableStream that streams over the argument, aCollection."
    self notImplementedYet.

- on: aCollection from: firstIndex to: lastIndex
    "Answer an instance of a kind of PositionableStream that streams over a copy of a subcollection of the argument,
     aCollection, from firstIndex to lastIndex."
    self notImplementedYet.

"category: testing."

- isEmpty
    "Answer true if the collection the receiver accesses has no elements; otherwise, answer false."
    self notImplementedYet.

"category: accessing."

- peek
    "Answer the next element in the collection (as in the message next), but do not change, the position reference.
     Answer nil if the receiver is at the end."
    self notImplementedYet.

- peekFor: anObject
    "Determine the response to the message peek. If it is the same as the argument, anObject, then increment the
    position reference and answer true. Otherwise answer false and do not change the position reference."
    self notImplementedYet.

- upTo: anObject
    "Answer a collection of elements starting with the next element accessed by the receiver, and up to, not inclusive
     of, the next element that is equal to anObject. If anObject is not in the collection, answer the entire rest of the
     collection."
    self notImplementedYet.

- reverseContents
    "Answer a copy of the receiver's contents in reverse order."
    self notImplementedYet.

"category: positioning."

- position
    "Answer the receiver's current position reference for accessing the collection."
    self notImplementedYet.

- position: anInteger
    "Set the receiver's current position reference for accessing the collection to be the argument, anInteger. If the
     argument is not within the bounds of the receiver's collection, report an error."
    self notImplementedYet.

- reset
    "Set the receiver's position reference to the beginning of the collection."
    self notImplementedYet.

- setToEnd
    "Set the receiver's position reference to the end of the collection."
    self notImplementedYet.

- skip: anInteger
    "Set the receiver's position reference to be the current position plus the argument, anInteger, possibly adjusting
     the result so as to remain within the bounds of the collection."
    self notImplementedYet.

- skipTo: anObject
    "Set the receiver's position reference to be past the next occurrence of the argument, anObject, in the collection.
     Answer whether such an occurrence existed."
    self notImplementedYet.
