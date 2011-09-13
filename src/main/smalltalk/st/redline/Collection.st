" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Collection
"instance creation"
+ with: anObject
    ^ self new
        add: anObject;
        yourself.

+ with: anObject with: anObject2
    ^ self new
        add: anObject;
        add: anObject2;
        yourself.

+ with: anObject with: anObject2 with: anObject3
    ^ self new
        add: anObject;
        add: anObject2;
        add: anObject3;
        yourself.

+ with: anObject with: anObject2 with: anObject3 with: anObject4
    ^ self new
        add: anObject;
        add: anObject2;
        add: anObject3;
        add: anObject4;
        yourself.

+ with: anObject with: anObject2 with: anObject3 with: anObject4 with: anObject5
    ^ self new
        add: anObject;
        add: anObject2;
        add: anObject3;
        add: anObject4;
        add: anObject5;
        yourself.

+ with: anObject with: anObject2 with: anObject3 with: anObject4 with: anObject5 with: anObject6
    ^ self new
        add: anObject;
        add: anObject2;
        add: anObject3;
        add: anObject4;
        add: anObject5;
        add: anObject6;
        yourself.

+ withAll: aCollection
    ^ self new
        addAll: aCollection;
        yourself.

"adding protocol"
- add: anObject
    self subclassResponsibility.

- addAll: aCollection
    aCollection do: [ :each | self add: each ].
    ^ aCollection.

"removing"
- remove: anObject ifAbsent: exceptionBlock
    self subclassResponsibility.

- remove: anObject
    self subclassResponsibility.

- removeAll: aCollection
    aCollection do: [ :each | self remove: each ].
    ^ aCollection

"private"
- errorNotFound
    self error: 'object is not in the collection'.

"testing"
- isEmpty
    ^ self size = 0.

- includes: anObject
    self do: [ :each | anObject == each ifTrue: [ ^ true ]].
    ^ false.
