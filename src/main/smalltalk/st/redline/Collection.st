" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Collection.

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

- species
    ^ self class.

- maxPrint
    ^ 5000.

"testing"

- isEmpty
    ^ self size = 0.

- includes: anObject
    self do: [ :each | anObject == each ifTrue: [ ^ true ]].
    ^ false.

- occurrencesOf: anObject
    | tally |
    tally := 0.
    self do: [:each | anObject == each ifTrue: [ tally := tally + 1 ]].
    ^ tally.

"enumerating"

- do: aBlock
    self subclassResponsibility.

- collect: aBlock
    | newCollection |
    newCollection := self species new.
    self do: [:each | newCollection add: (aBlock value: each)].
    ^ newCollection.

- detect: aBlock
    ^ self detect: aBlock ifNone: [self errorNotFound].

- detect: aBlock ifNone: exceptionBlock
    self do: [:each | (aBlock value: each) ifTrue: [^ each]].
    ^ exceptionBlock value.

- inject: thisValue into: binaryBlock
    | nextValue |
    nextValue := thisValue.
    self do: [:each | nextValue := binaryBlock value: nextValue value: each].
    ^ nextValue.

- reject: aBlock
    ^ self select: [:element | (aBlock value: element) == false].

- select: aBlock
    | newCollection |
    newCollection := self species new.
    self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
    ^ newCollection.

"accessing"

- size
    | tally |
    tally := 0.
    self do: [:each | tally := tally + 1].
    ^ tally.

"converting"

- asBag
    | aBag |
    aBag := Bag new.
    self do: [:each | aBag add: each].
    ^ aBag.

- asOrderedCollection
    | anOrderedCollection |
    anOrderedCollection := OrderedCollection new.
    self do: [:each | anOrderedCollection add: each].
    ^ anOrderedCollection.

- asSet
    | aSet |
    aSet := Set new: self size.
    self do: [:each | aSet add: each].
    ^ aSet.

- asSortedCollection
    | aSortedCollection |
    aSortedCollection := SortedCollection new: self size.
    aSortedCollection addAll: self.
    ^ aSortedCollection.

- asSortedCollection: aSortBlock
    | aSortedCollection |
    aSortedCollection := SortedCollection new: self size.
    aSortedCollection sortBlock: aSortBlock.
    aSortedCollection addAll: self.
    ^ aSortedCollection.

"printing"
- printOn: aStream
    | tooMany |
    tooMany := aStream position + self maxPrint.
    aStream nextPutAll: self class name.
    aStream nextPutAll: '('.
    self do:
      [ :element |
        aStream position \> tooMany
          ifTrue: [ aStream nextPutAll: '...etc...)'. ^ self ].
        element printOn: aStream.
        aStream space ].
    aStream nextPutAll: ')'.

- storeOn: aStream
    | noneYet |
    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: 'new)'.
    noneYet := true.
    self do:
      [ :each |
        noneYet
          ifTrue: [ noneYet := false ]
          ifFalse: [ aStream nextPutAll: ';' ].
        aStream nextPutAll: 'add'.
        aStream store: each ].
    noneYet ifFalse: [ aStream nextPutAll: '; yourself' ].
    aStream nextPutAll: ')'.
