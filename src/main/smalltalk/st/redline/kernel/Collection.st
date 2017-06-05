"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I am the abstract superclass of all classes that represent a group of elements."

Object subclass: #Collection
       instanceVariableNames: ''
       classVariableNames: 'RandomForPicking '
       poolDictionaries: ''
       category: 'Collections-Abstract'.

"Collection methodsFor: 'accessing'"

- size
    "Answer how many elements the receiver contains."

    | tally |
    tally := 0.
    self do: [:each | tally := tally + 1].
    ^ tally.

"Collection methodsFor: 'testing'"

- includes: anObject
    "Answer whether anObject is one of the receiver's elements."

    self do: [:each | anObject = each ifTrue: [^ true]].
    ^ false.

- includesAllOf: aCollection
    "Answer whether all the elements of aCollection are in the receiver."
    aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
    ^ true.

- includesAnyOf: aCollection
    "Answer whether any element of aCollection is one of the receiver's elements."
    aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
    ^ false.

- includesSubstringAnywhere: testString
    "Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
    self do:
      [:element |
        (element isKindOf: String)
          ifTrue:
            [(element includesSubString: testString) ifTrue: [^ true]].
        (element isKindOf: Collection)
          ifTrue:
            [(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
    ^ false.


- isEmpty
	"Answer whether the receiver contains any elements."

	^ self size = 0.

- occurrencesOf: anObject
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^ tally.

"Collection methodsFor: 'adding'.

- add: newObject
    "Include newObject as one of the receiver's elements. Answer newObject.
    ArrayedCollections cannot respond to this message."

    self subclassResponsibility.

- addAll: aCollection
    "Include all the elements of aCollection as the receiver's elements. Answer
    aCollection."

    aCollection do: [:each | self add: each].
    ^ aCollection.

- addIfNotPresent: anObject

	(self includes: anObject) ifFalse: [^ self add: anObject].


"Collection methodsFor: 'removing'"

- remove: oldObject
    "Remove oldObject as one of the receiver's elements. Answer oldObject
    unless no element is equal to oldObject, in which case, create an error
    notification."

    ^ self remove: oldObject ifAbsent: [self errorNotFound].

- remove: oldObject ifAbsent: anExceptionBlock
    "Remove oldObject as one of the receiver's elements. If several of the
    elements are equal to oldObject, only one is removed. If no element is
    equal to oldObject, answer the result of evaluating anExceptionBlock.
    Otherwise, answer the argument, oldObject. SequenceableCollections
    cannot respond to this message."

    self subclassResponsibility.

- removeAll: aCollection
    "Remove each element of aCollection from the receiver. If successful for
    each, answer aCollection. Otherwise create an error notification."

    aCollection do: [:each | self remove: each].
    ^ aCollection.

- removeAllFoundIn: aCollection
    "Remove each element of aCollection which is present in the receiver from the receiver"

    aCollection do: [:each | self remove: each ifAbsent: []].
    ^ aCollection.

- removeAllSuchThat: aBlock
    "Apply the condition to each element and remove it if the condition is true.  Use a copy to enumerate collections
    whose order changes when an element is removed (Set)."

    | copy newCollection |
    newCollection := self species new.
    copy := self copy.
    copy do: [:element |
      (aBlock value: element) ifTrue: [
        self remove: element.
        newCollection add: element]].
    ^ newCollection.


"Collection methodsFor: 'enumerating'"

- associationsDo: aBlock
    "Evaluate aBlock for each of the receiver's elements (key/value
    associations).  If any non-association is within, the error is not caught now,
    but later, when a key or value message is sent to it."

    self do: aBlock.

- average
	"Return the average of all my elements."

	^ self sum asFloat / self size.

- collect: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Collect the resulting values into a collection like the receiver. Answer
    the new collection."

    | newCollection |
    newCollection := self species new.
    self do: [:each | newCollection add: (aBlock value: each)].
    ^ newCollection.

- collect: collectBlock thenSelect: selectBlock
	^ (self collect: collectBlock) select: selectBlock.

- count: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.  Return the number that answered true."

    | sum |
    sum := 0.
    self do: [:each |
      (aBlock value: each) ifTrue: [sum := sum + 1]].
    ^ sum.

- detect: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Answer the first element for which aBlock evaluates to true."

    ^ self detect: aBlock ifNone: [self errorNotFound].

- detect: aBlock ifNone: exceptionBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Answer the first element for which aBlock evaluates to true. If none
    evaluate to true, then evaluate the argument, exceptionBlock."

    self do: [:each | (aBlock value: each) ifTrue: [^ each]].
    ^ exceptionBlock value.

- detectMax: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Answer the element for which aBlock evaluates to the highest magnitude.
    If collection empty, return nil.  This method might also be called elect:."

    | maxElement maxValue val |
    self do: [:each |
      maxValue == nil
        ifFalse: [
          (val := aBlock value: each) > maxValue ifTrue: [
            maxElement := each.
            maxValue := val]]
        ifTrue: ["first element"
          maxElement := each.
          maxValue := aBlock value: each].
          "Note that there is no way to get the first element that works
          for all kinds of Collections.  Must test every one."].
    ^ maxElement.

- detectMin: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Answer the element for which aBlock evaluates to the lowest number.
    If collection empty, return nil."

    | minElement minValue val |
    self do: [:each |
      minValue == nil
        ifFalse: [
          (val := aBlock value: each) < minValue ifTrue: [
            minElement := each.
            minValue := val]]
        ifTrue: ["first element"
          minElement := each.
          minValue := aBlock value: each].
          "Note that there is no way to get the first element that works
          for all kinds of Collections.  Must test every one."].
    ^ minElement.

- detectSum: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Return the sum of the answers."
    | sum |
    sum := 0.
    self do: [:each |
      sum := (aBlock value: each) + sum].
    ^ sum.

- do: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument."

    self subclassResponsibility.

- inject: thisValue into: binaryBlock
    "Accumulate a running value associated with evaluating the argument,
    binaryBlock, with the current value of the argument, thisValue, and the
    receiver as block arguments. For instance, to sum the numeric elements
    of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal +
    next]."

    | nextValue |
    nextValue := thisValue.
    self do: [:each | nextValue := binaryBlock value: nextValue value: each].
    ^ nextValue.

- max
    "Return the max of all my elements."
    | max | max := nil.
    self do: [:each | (max == nil or: [each > max])
            ifTrue: [max := each]].
    ^ max.

- reject: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Collect into a new collection like the receiver only those elements for
    which aBlock evaluates to false. Answer the new collection."

    ^ self select: [:element | (aBlock value: element) == false].

- select: aBlock
    "Evaluate aBlock with each of the receiver's elements as the argument.
    Collect into a new collection like the receiver, only those elements for
    which aBlock evaluates to true. Answer the new collection."

    | newCollection |
    newCollection := self species new.
    self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
    ^ newCollection.

- select: selectBlock thenCollect: collectBlock
	^ (self select: selectBlock) collect: collectBlock.

- sum
    "Return the sum of all my elements."
    | sum |  sum := 0.
    self do: [:each | sum := sum + each].
    ^ sum.


"Collection methodsFor: 'converting'"

- asBag
    "Answer a Bag whose elements are the elements of the receiver."

    | aBag |
    aBag := Bag new.
    self do: [:each | aBag add: each].
    ^ aBag.

- asOrderedCollection
    "Answer an OrderedCollection whose elements are the elements of the
    receiver. The order in which elements are added depends on the order in
    which the receiver enumerates its elements. In the case of unordered
    collections, the ordering is not necessarily the same for multiple requests
    for the conversion."

    | anOrderedCollection |
    anOrderedCollection := OrderedCollection new: self size.
    self do: [:each | anOrderedCollection addLast: each].
    ^ anOrderedCollection.

- asSet
    "Answer a Set whose elements are the unique elements of the receiver."

    | aSet |
    aSet := Set new: self size.
    self do: [:each | aSet add: each].
    ^ aSet.

- asSortedArray
    "Return a copy of the receiver in sorted order, as an Array."

    ^ self asSortedCollection asArray.

- asSortedCollection
    "Answer a SortedCollection whose elements are the elements of the
    receiver. The sort order is the default less than or equal."

    | aSortedCollection |
    aSortedCollection := SortedCollection new: self size.
    aSortedCollection addAll: self.
    ^ aSortedCollection.

- asSortedCollection: aBlock
    "Answer a SortedCollection whose elements are the elements of the
    receiver. The sort order is defined by the argument, aBlock."

    | aSortedCollection |
    aSortedCollection := SortedCollection new: self size.
    aSortedCollection sortBlock: aBlock.
    aSortedCollection addAll: self.
    ^ aSortedCollection.


"Collection methodsFor: 'printing'"

- printOn: aStream
    "Refer to the comment in Object|printOn:."
    aStream nextPutAll: self class name + ' ('.
    self do: [:element | element printOn: aStream. aStream space].
    aStream nextPut: $).

- storeOn: aStream
    "Refer to the comment in Object|storeOn:."

    | noneYet |
    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: ' new)'.
    noneYet := true.
    self do:
      [:each |
      noneYet
        ifTrue: [noneYet := false]
        ifFalse: [aStream nextPut: $;].
      aStream nextPutAll: ' add: '.
      aStream store: each].
    noneYet ifFalse: [aStream nextPutAll: '; yourself'].
    aStream nextPut: $).


"Collection methodsFor: 'private'"
- emptyCheck

	  self isEmpty ifTrue: [self errorEmptyCollection].

- errorEmptyCollection

    self error: 'this collection is empty'.

- errorNoMatch

    self error: 'collection sizes do not match'.

- errorNotFound

    self error: 'Object is not in the collection.'.

- errorNotKeyed

    self error: self class name + 's do not respond to keyed accessing messages.'.

- fill: numElements fromStack: aContext
    "Fill me with numElements elements, popped in reverse order from
     the stack of aContext.  Do not call directly: this is called indirectly by {1. 2. 3}
     constructs.  Subclasses that support at:put: instead of add: should override
     this and call Context<pop:toIndexable:"

    aContext pop: numElements toAddable: self.

- maxSize
    "Answer the largest basicSize which is valid for the receiver's class."

    ^ 65486. "for VM3 interpreter DoradoST80Aug19"

- toBraceStack: itsSize
    "Push receiver's elements onto the stack of thisContext sender.  Error if receiver does
     not have itsSize elements or if receiver is unordered.
     Do not call directly: this is called by {a. b} _ ... constructs."

    self size ~= itsSize ifTrue:
      [self error: 'Trying to store ' + self size printString,
            ' values into ' + itsSize printString, ' variables.'].
    thisContext sender push: itsSize fromIndexable: self.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

+ fromBraceStack: itsSize
    "Answer an instance of me with itsSize elements, popped in reverse order from
     the stack of thisContext sender.  Do not call directly: this is called by {1. 2. 3}
     constructs."

    ^ self newFrom: ((Array new: itsSize) fill: itsSize fromStack: thisContext sender).

"Collection class methodsFor: 'instance creation'"

+ with: anObject
	"Answer an instance of me containing anObject."

	| newCollection |
	newCollection := self new.
	newCollection add: anObject.
	^ newCollection.

+ with: firstObject with: secondObject
    "Answer an instance of me containing the two arguments as elements."

    | newCollection |
    newCollection := self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    ^ newCollection.

- with: firstObject with: secondObject with: thirdObject
    "Answer an instance of me containing the three arguments as elements."

    | newCollection |
    newCollection := self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    ^ newCollection.

+ with: firstObject with: secondObject with: thirdObject with: fourthObject
    "Answer an instance of me, containing the four arguments as the
    elements."

    | newCollection |
    newCollection := self new.
    newCollection add: firstObject.
    newCollection add: secondObject.
    newCollection add: thirdObject.
    newCollection add: fourthObject.
    ^ newCollection.


"Collection class methodsFor: 'private'"

+ initialize
	"Set up a Random number generator to be used by pickOne when the user does not feel like creating his own Random generator."
	RandomForPicking := Random new.
