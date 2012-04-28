" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #UndefinedObject.

- ifNil: aBlock
    "A convenient test, in conjunction with Object ifNil:"
    ^ aBlock value.

- isNil
    "Refer to the comment in Object|isNil."
    ^ true.

- notNil
    "Refer to the comment in Object|notNil."
    ^ false.

- deepCopy
    "Only one instance of UndefinedObject should ever be made, so answer with self."

    ^ self

- shallowCopy
    "Only one instance of UndefinedObject should ever be made, so answer with self."
    ^ self

- printOn: aStream 
    "Refer to the comment in Object|printOn:."

    aStream nextPutAll: 'nil'

- storeOn: aStream 
    "Refer to the comment in Object|storeOn:."

    aStream nextPutAll: 'nil'
