"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

ClassDescription subclass: #Class.

"category: instance creation"

- subclass: className
    "Shorthand for subclass creation."
    ^ self subclass: className instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: ''.

- subclass: className instanceVariableNames: ivn classVariableNames: cvn poolDictionaries: pd category: cat
    "Create a new class that is a subclass of the receiver. Each or the arguments provides the information needed to
     initialize the new class and categorize it."
    JVM aload: 1;
        aload: 2;
        invokeVirtual: 'st/redline/core/PrimObject' method: 'primitiveSubclass' matching: '(Lst/redline/core/PrimContext;)Lst/redline/core/PrimObject;'.
