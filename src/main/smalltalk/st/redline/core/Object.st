"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

PrimObject subclass: #Object.

"category: initialise-release"

- initialize
    "Subclasses should redefine this method to perform initializations on instance creation"
    ^ self.

"category: accessing"

- yourself
    "Answer the receiver."
    ^ self.

"category: class membership"

- class
    "Primitive. Answer the object which is the receiver's class."
    JVM aload: 1;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'selfClass' matching: '()Lst/redline/core/PrimObject;'.
