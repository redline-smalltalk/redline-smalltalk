"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I am an abstract collection of elements with a fixed range of integers (from 1 to n>=1) as external keys."

SequenceableCollection subclass: #ArrayedCollection
                       category: 'Collections-Abstract'.

"ArrayedCollection methodsFor: 'accessing'"

- size
    "Primitive. Answer the number of indexable fields in the receiver. This
	  value is the same as the largest legal subscript. Primitive is specified
	  here to override SequenceableCollection size. Essential. See Object
	  documentation whatIsAPrimitive. "

	  JVM primitive: 62.

"ArrayedCollection methodsFor: 'adding'"

- add: newObject

	self shouldNotImplement.


"ArrayedCollection methodsFor: 'printing'"

- storeOn: aStream

    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: ' new: '.
    aStream store: self size.
    aStream nextPut: $).
    (self storeElementsFrom: 1 to: self size on: aStream)
      ifFalse: [aStream nextPutAll: '; yourself'].
    aStream nextPut: $).


"ArrayedCollection methodsFor: 'private'"

- defaultElement

    ^ nil.

- fill: numElements fromStack: aContext
    "Fill me with numElements elements, popped in reverse order from
	  the stack of aContext.  Do not call directly: this is called indirectly by {1. 2. 3}
	  constructs."

    aContext pop: numElements toIndexable: self.

- storeElementsFrom: firstIndex to: lastIndex on: aStream

    | noneYet defaultElement arrayElement |
    noneYet := true.
    defaultElement := self defaultElement.
    firstIndex to: lastIndex do:
      [:index |
      arrayElement := self at: index.
      arrayElement = defaultElement
        ifFalse:
          [noneYet
            ifTrue: [noneYet := false]
            ifFalse: [aStream nextPut: $;].
          aStream nextPutAll: ' at: '.
          aStream store: index.
          aStream nextPutAll: ' put: '.
          aStream store: arrayElement]].
    ^ noneYet.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"ArrayedCollection class methodsFor: 'instance creation'"

+ fromBraceStack: itsSize
    "Answer an instance of me with itsSize elements, popped in reverse order from
	  the stack of thisContext sender.  Do not call directly: this is called by {1. 2. 3}
	  constructs."

	  ^ (self new: itsSize) fill: itsSize fromStack: thisContext sender.

+ new
    "Answer a new instance of me, with size = 0."

	  ^ self new: 0.

+ new: size withAll: value
    "Answer an instance of me, with number of elements equal to size, each
	  of which refers to the argument, value."

    ^ (self new: size) atAllPut: value.

+ newFrom: aCollection
    "Answer an instance of me containing the same elements as aCollection."

    | newArray |
    newArray := self new: aCollection size.
    1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
    ^ newArray.

+ with: anObject
    "Answer a new instance of me, containing only anObject."

    | newCollection |
    newCollection := self new: 1.
    newCollection at: 1 put: anObject.
    ^ newCollection.

+ with: firstObject with: secondObject
    "Answer a new instance of me, containing firstObject and secondObject."

    | newCollection |
    newCollection := self new: 2.
    newCollection at: 1 put: firstObject.
    newCollection at: 2 put: secondObject.
    ^ newCollection.

+ with: firstObject with: secondObject with: thirdObject
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^ newCollection.

+ with: firstObject with: secondObject with: thirdObject with: fourthObject
    "Answer a new instance of me, containing only the three arguments as
    elements."

    | newCollection |
    newCollection := self new: 4.
    newCollection at: 1 put: firstObject.
    newCollection at: 2 put: secondObject.
    newCollection at: 3 put: thirdObject.
    newCollection at: 4 put: fourthObject.
    ^ newCollection.

+ with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
    "Answer a new instance of me, containing only the five arguments as
    elements."

    | newCollection |
    newCollection := self new: 5.
    newCollection at: 1 put: firstObject.
    newCollection at: 2 put: secondObject.
    newCollection at: 3 put: thirdObject.
    newCollection at: 4 put: fourthObject.
    newCollection at: 5 put: fifthObject.
    ^ newCollection.
