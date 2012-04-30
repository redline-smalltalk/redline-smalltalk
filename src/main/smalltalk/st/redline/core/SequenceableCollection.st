" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Collection < #SequenceableCollection.

"accessing"

- size
    <primitive: 220>

- at: index
    <primitive: 222>

- at: index put: anObject
    <primitive: 223>

- basicAt: index
    <primitive: 222>

- basicAt: index put: anObject
    <primitive: 223>

"removing"

- remove: oldObject ifAbsent: anExceptionBlock
    self shouldNotImplement.

"enumerating"

- do: aBlock
    1 to: self size do: [ :index |
      aBlock value: (self at: index)
    ].

- collect: aBlock
    | aStream |
    aStream := WriteStream on: (self species new: self size).
    1 to: self size do: [ :index |
      aStream nextPut: (aBlock value: (self at: index))
    ].
    ^ aStream contents.

- select: aBlock
    | aStream |
    aStream := WriteStream on: (self species new: self size).
    1 to: self size do: [ :index |
      (aBlock value: (self at: index))
        ifTrue: [ aStream nextPut: (self at: index) ]
    ].
    ^ aStream contents.
