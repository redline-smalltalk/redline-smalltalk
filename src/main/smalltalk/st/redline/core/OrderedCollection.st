"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

SequenceableCollection subclass: #OrderedCollection.

"category: accessing"

- after: oldObject
    "Answer the element after old0bject in the receiver. If the receiver does not contain oldObject or if the receiver
     contains no elements after oldObject, report an error."
    self notImplementedYet.

- before: oldObject
    "Answer the element before oldObject in the receiver. If the receiver does not contain oldObject or if the receiver
     contains no elements before oldObject, report an error."
    self notImplementedYet.

"category: adding"

- add: newObject after: oldObject
    "Add the argument, newObject, as an element of the receiver. Put it in the sequence just succeeding oldObject.
     Answer newObject. If oldObject is not found, then report an error."
    self notImplementedYet.

- add: newObject before: oldObject
    "Add the argument, newObject, as an element of the receiver. Put it in the sequence just preceding oldObject.
    Answer newObject. If oldObject is not found, then report an error."
    self notImplementedYet.

- addAddFirst: anOrderedCollection
    "Add each element of the argument, anOrderedCollection, at the beginning of the receiver. Answer
    anOrderedCollection."
    self notImplementedYet.

- addAllLast: anOrderedCollection
    "Add each element of the argument, anOrderedCollection, to the end of the receiver. Answer anOrderedCollection."
    self notImplementedYet.

- addFirst: newObject
    "Add the argument, newObject, to the beginning of the receiver. Answer newObject."
    self notImplementedYet.

- addLast: newObject
    "Add the argument, newObject, to the end of the receiver. Answer newObject."
    self notImplementedYet.

"category: removing"

- removeFirst
    "Remove the first element of the receiver and answer it. If the receiver is empty, report an error."
    self notImplementedYet.

- removeLast
    "Remove the last  element of the receiver and answer it. If the receiver is empty, report an error."
    self notImplementedYet.
