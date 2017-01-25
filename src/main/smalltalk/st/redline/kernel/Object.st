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
