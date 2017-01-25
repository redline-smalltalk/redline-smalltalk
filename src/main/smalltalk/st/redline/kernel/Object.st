"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

PrimObject subclass: #Object.

"category: initialisation"

- initialize
    "Subclasses should redefine this method to perform initializations on instance creation"
    ^ self.

"category: evaluating"

- value
    ^ self.

"category: accessing"

- class
    "Primitive. Answer the object which is the receiver's class."
    JVM primitive: 111.

- yourself
    ^ self.

- at: index
    "Answer the value of the indexed instance variable of the receiver whose index is the argument, index. If the
     receiver does not have indexed variables, or if the argument is greater than the number of indexed variables, then
     report an error."
    JVM primitive: 144.

- at: index put: anObject
    "Store the argument, anObject, as the value of the indexed instance variable of the receiver whose index is the
     argument, index. If the receiver does not have indexed variables, or if the argument is greater than the number of
     indexed variables, then report an error. Answer anObject."
    JVM primitive: 143.

- size
    "Answer the receiver's number of indexed variables. This value is the same as the largest legal index."
    JVM primitive: 62.

- basicSize
    "Same as size. The method associated with this message, however, cannot be modified in any subclass."
    JVM primitive: 62.
