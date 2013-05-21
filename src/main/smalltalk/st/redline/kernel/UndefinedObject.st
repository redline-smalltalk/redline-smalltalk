"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #UndefinedObject.

"category: testing"

- isNil
    "Refer to the comment in Object|isNil."
    ^ true.

- notNil
    "Refer to the comment in Object|notNil."
    ^ false.

- ifNil: aBlock
    "A convenient test, in conjunction with Object ifNil:"
    ^ aBlock value.

- ifNotNil: aBlock
    "A convenient test, in conjunction with Object ifNil:"
    ^ self.

"category: copying"

- deepCopy
    "Only one instance of UndefinedObject should ever be made, so answer with self."
    ^ self.

- shallowCopy
    "Only one instance of UndefinedObject should ever be made, so answer with self."
    ^ self.

"category: printing"

- printOn: aStream
    "Refer to the comment in Object|printOn:."
    aStream nextPutAll: 'nil'.

- storeOn: aStream
    "Refer to the comment in Object|storeOn:."
    aStream nextPutAll: 'nil'.
