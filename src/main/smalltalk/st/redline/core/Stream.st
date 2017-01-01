"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Stream.

"category: accessing - reading"

- next
    "Answer the next object accessible by the receiver."
    self notImplementedYet.

- next: anInteger
    "Answer the next anlnteger number of objects accessible by the receiver. Generally, the answer will be a collection
     of the same class as the one accessed by the receiver."
    self notImplementedYet.

- nextMatchFor: anObject
    "Access the next object and answer whether it is equal to the argument, anObject."
    self notImplementedYet.

- contents
    "Answer all of the objects in the collection accessed by the receiver. Generally, the answer will be a collection
     of the same class as the one accessed by the receiver."
    self notImplementedYet.

"category: accessing - writing"

- nextPut: anObject
    "Store the argument, anObject, as the next element accessible by the receiver. Answer anObject."
    self notImplementedYet.

- nextPutAll: aCollection
    "Store the elements in the argument, aCollection, as the next elements accessible by the receiver. Answer
     aCollection."
    self notImplementedYet.

- next: anInteger put: anObject
    "Store the argument, anObject, as the next anInteger number of elements accessible by the receiver. Answer
    anObject."
    self notImplementedYet.

"category: testing"

- atEnd
    "Answer whether the receiver cannot access any more objects."
    self notImplementedYet.

- do: aBlock
    "Evaluate the argument, aBlock, for each of the remaining elements that can be accessed by the receiver."
    self notImplementedYet.
