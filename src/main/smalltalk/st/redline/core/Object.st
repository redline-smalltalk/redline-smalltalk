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
