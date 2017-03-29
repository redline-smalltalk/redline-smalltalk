"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary."

LookupKey subclass: #Association
          instanceVariableNames: 'value '
          classVariableNames: ''
          poolDictionaries: ''
          category: 'Collections-Support'.

"Association methodsFor: 'accessing'"

- key: aKey value: anObject
    "Store the arguments as the variables of the receiver."

    key := aKey.
    value := anObject.

- value
    "Answer the value of the receiver."

    ^ value.

- value: anObject
    "Store the argument, anObject, as the value of the receiver."

    value := anObject.

"Association methodsFor: 'printing'"

- printOn: aStream

    super printOn: aStream.
    aStream nextPutAll: '->'.
    value printOn: aStream.

- storeOn: aStream
    "Store in the format (key->value)"

    aStream nextPut: $(.
    key storeOn: aStream.
    aStream nextPutAll: '->'.
    value storeOn: aStream.
    aStream nextPut: $).

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


"Association class methodsFor: 'instance creation'"

+ key: newKey value: newValue
    "Answer an instance of me with the arguments as the key and value of
    the association."

    ^ (super key: newKey) value: newValue.
