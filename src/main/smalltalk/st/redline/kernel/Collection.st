"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #Collection.

"category: adding"

- add: newObject
    "Include the argument , newObject, as one of the receiver's elements. Answer newObject."
    self notImplementedYet.

- addAll: aCollection
    "Include all the elements of the argument, aCollection, as the receiver's elements. Answer aCollection."
    self notImplementedYet.

"category: removing"

- remove: oldObject
    "Remove the argument, oldObject, from the receiver's elements. Answer oldObject unless no element is equal to
     oldObject, in which case, report that an error occurred."
    self notImplementedYet.

- remove: oldObject ifAbsent: anExceptionBlock
    "Remove the argument , oldObject, from the receiver's elements. If several of the elements are equal to oldObject,
     only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBlock.
     Otherwise, answer oldObject."
    self notImplementedYet.

- removeAll: aCollection
    "Remove each element of the argument, aCollection, from the receiver. If successful for each, answer aCollection.
     Otherwise report that an error occurred."
    self notImplementedYet.

- includes: anObject
    "Answer whether the argument , anObject, is equal to one of the receiver's elements."
    self notImplementedYet.

- isEmpty
    "Answer whether the receiver contains any elements."
    self notImplementedYet.

- occurrencesOf: anObject
    "Answer how many of the receiver's elements are equal to the argument , anObject."
    self notImplementedYet.

"category: enumeration"

- do: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements."
    self notImplementedYet.

- select: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements. Collect into a new collection like that of the
     receiver, only those elements for which aBlock evaluates to true. Answer the new collection."
    self notImplementedYet.

- reject: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements. Collect into a new collection like that of the
     receiver only those elements for which aBlock evaluates to false. Answer the new collection."
    self notImplementedYet.

- collect: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements. Answer a new collection like that of the
     receiver containing the values returned by the block on each evaluation."
    self notImplementedYet.

- detect: aBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements. Answer the first element for which aBlock
     evaluates to true. If none evaluates to true, report an error."
    self notImplementedYet.

- detect: aBlock ifNone: exceptionBlock
    "Evaluate the argument, aBlock, for each of the receiver's elements. Answer the first element for which aBlock
     evaluates to true. If none evaluates to true, evaluate the argument, exceptionBlock, exceptionBlock must be a
     block requiring no arguments."
    self notImplementedYet.

- inject: thisValue into: binaryBlock
    "Evaluate the argument, binaryBlock, once for each element in the receiver. The block has two arguments: the second
     is an element from the receiver; the first is the value of the previous evaluation of the block, starting with the
     argument, thisValue. Answer the final value of the block."
    self notImplementedYet.

"category: instance creation"

+ with: anObject
    "Answer an instance of the collection containing anObject."
    self notImplementedYet.

+ with: firstObject with: secondObject
    "Answer an instance of the collection containing firstObject and secondObject as elements."
    self notImplementedYet.

+ with: firstObject with: secondObject with: thirdObject
    "Answer an instance of the collection containing firstObject, secondObject, and thirdObject as elements."
    self notImplementedYet.

+ with: firstObject with: secondObject with: thirdObject with: fourthObject
    "Answer an instance of the collection, containing firstObject, secondObject, thirdObject, and fourthObject as the
     elements."
    self notImplementedYet.

"category: converting"

- asBag
    "Answer a Bag whose elements are those of the receiver."
    self notImplementedYet.

- asSet
    "Answer a Set whose elements are those of the receiver (any duplications are therefore eliminated)."
    self notImplementedYet.

- asOrderedCollection
    "Answer an OrderedCollection whose elements are those of the receiver (ordering is possibly arbitrary)."
    self notImplementedYet.

- asSortedCollection
    "Answer a SortedCollection whose elements are those of the receiver, sorted so that each element is less than or
     equal to (<=) its successors."
    self notImplementedYet.

- asSortedCollection: aBlock
    "Answer a SortedCollection whose elements are those of the receiver, sorted according to the argument aBlock."
    self notImplementedYet.
