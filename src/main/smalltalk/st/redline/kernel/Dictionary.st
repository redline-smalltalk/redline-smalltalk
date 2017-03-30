"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a set of elements that can be viewed from one of two perspectives: a set of associations, or a set of values
 that are externally named where the name can be any object that responds to =. The external name is referred to as the
 key."

Set subclass: #Dictionary
  	category: 'Collections-Unordered'.

"Dictionary methodsFor: 'accessing'"

- associationAt: key
  	^ self associationAt: key ifAbsent: [self errorKeyNotFound].

- associationAt: key ifAbsent: aBlock
    "Answer the association with the given key.
    If key is not found, return the result of evaluating aBlock."

    | index assoc |
    index := self findElementOrNil: key.
    assoc := array at: index.
    nil == assoc ifTrue: [ ^ aBlock value ].
    ^ assoc.

- at: key
    "Answer the value associated with the key."

    ^ self at: key ifAbsent: [self errorKeyNotFound].

- at: key ifAbsent: aBlock

    | index assoc |
    index := self findElementOrNil: key.
    assoc := array at: index.
    nil == assoc ifTrue: [ ^ aBlock value ].
    ^ assoc value.

- at: key ifPresent: aBlock
    "Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the
    value associated with the key. Otherwise, answer nil."

    | v |
    v := self at: key ifAbsent: [^ nil].
    ^ aBlock value: v.

- at: key put: anObject
    "Set the value at key to be anObject.  If key is not found, create a new
    entry for key and set is value to anObject. Answer anObject."
    | index element |
    index := self findElementOrNil: key.
    element := array at: index.
    element == nil
      ifTrue: [self atNewIndex: index put: (Association key: key value: anObject)]
      ifFalse: [element value: anObject].
    ^ anObject.

- keyAtValue: value
    "Answer the key that is the external name for the argument, value. If
    there is none, answer nil."

    ^ self keyAtValue: value ifAbsent: [self errorValueNotFound].

- keyAtValue: value ifAbsent: exceptionBlock
    "Answer the key that is the external name for the argument, value. If
    there is none, answer the result of evaluating exceptionBlock.
    : Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."

    self associationsDo:
      [:association | value = association value ifTrue: [^ association key]].
    ^ exceptionBlock value.

- keys
    "Answer a Set containing the receiver's keys."
    | aSet |
    aSet := Set new: self size.
    self keysDo: [:key | aSet add: key].
    ^ aSet.


"Dictionary methodsFor: 'testing'"

- includes: anObject

    self do: [:each | anObject = each ifTrue: [^ true]].
    ^ false! !

- includesKey: key
    "Answer whether the receiver has a key equal to the argument, key."
    | index |
    index := self findElementOrNil: key.
    (array at: index) == nil
      ifTrue: [^ false]
      ifFalse: [^ true].

- occurrencesOf: anObject
    "Answer how many of the receiver's elements are equal to anObject."

    | count |
    count := 0.
    self do: [:each | anObject = each ifTrue: [count := count + 1]].
    ^ count.

"Dictionary methodsFor: 'adding'"

- add: anAssociation
	| index element |
	index := self findElementOrNil: anAssociation key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: anAssociation]
		ifFalse: [element value: anAssociation value].
	^ anAssociation.

- declare: key from: aDictionary
    "Add key to the receiver. If key already exists, do nothing. If aDictionary
    includes key, then remove it from aDictionary and use its association as
    the element of the receiver."

    (self includesKey: key) ifTrue: [^ self].
    (aDictionary includesKey: key)
      ifTrue:
        [self add: (aDictionary associationAt: key).
        aDictionary removeKey: key]
      ifFalse:
        [self add: key -> nil].


"Dictionary methodsFor: 'removing'"

- remove: anObject

	  self shouldNotImplement.

- remove: anObject ifAbsent: exceptionBlock

  	self shouldNotImplement.

- removeKey: key
    "Remove key from the receiver.
    If key is not in the receiver, notify an error."

    ^ self removeKey: key ifAbsent: [self errorKeyNotFound].

- removeKey: key ifAbsent: aBlock
    "Remove key (and its associated value) from the receiver. If key is not in
    the receiver, answer the result of evaluating aBlock. Otherwise, answer
    the value externally named by key."

    | index assoc |
    index := self findElementOrNil: key.
    assoc := array at: index.
    assoc == nil ifTrue: [ ^ aBlock value ].
    array at: index put: nil.
    tally := tally - 1.
    self fixCollisionsFrom: index.
    ^ assoc value.

- removeUnreferencedKeys

	^ self unreferencedKeys do: [:key | self removeKey: key].

- unreferencedKeys

    | n |
    ^ 'Scanning for references . . .'
      displayProgressAt: Sensor cursorPoint
      from: 0 to: self size
      during:
      [:bar | n := 0.
      self keys select:
        [:key | bar value: (n := n+1).
        (Smalltalk allCallsOn: (self associationAt: key)) isEmpty]].


"Dictionary methodsFor: 'enumerating'"

- associationsDo: aBlock
    "Evaluate aBlock for each of the receiver's elements (key/value
    associations)."

    super do: aBlock.

- collect: aBlock
    "Evaluate aBlock with each of my values as the argument.  Collect the
    resulting values into a collection that is like me. Answer with the new
    collection."

    | newCollection |
    newCollection := OrderedCollection new: self size.
    self do: [:each | newCollection add: (aBlock value: each)].
    ^ newCollection.

- do: aBlock

    super do: [:assoc | aBlock value: assoc value].

- keysDo: aBlock
    "Evaluate aBlock for each of the receiver's keys."

    self associationsDo: [:association | aBlock value: association key].

- select: aBlock
    "Evaluate aBlock with each of my values as the argument. Collect into a
    new dictionary, only those associations for which aBlock evaluates to
    true."

    | newCollection |
    newCollection := self species new.
    self associationsDo:
      [:each |
      (aBlock value: each value) ifTrue: [newCollection add: each]].
    ^ newCollection.


"Dictionary methodsFor: 'printing'"

- printOn: aStream
    aStream nextPutAll: self class name + ' ('.
    self associationsDo: [:element | element printOn: aStream. aStream space].
    aStream nextPut: $).

- storeOn: aStream
    | noneYet |
    aStream nextPutAll: '(('.
    aStream nextPutAll: self class name.
    aStream nextPutAll: ' new)'.
    noneYet := true.
    self associationsDo:
        [:each |
        noneYet
          ifTrue: [noneYet := false]
          ifFalse: [aStream nextPut: $;].
        aStream nextPutAll: ' add: '.
        aStream store: each].
    noneYet ifFalse: [aStream nextPutAll: '; yourself'].
    aStream nextPut: $).


"Dictionary methodsFor: 'private'"

- copy
    "Must copy the associations, or later store will effect both the original and the copy"

    ^ self shallowCopy withArray:
      (array collect: [:assoc |
        assoc ifNil: [nil]
          ifNotNil: [Association key: assoc key
  value: assoc value]]).

- errorKeyNotFound

    self error: 'key not found'.

- errorValueNotFound

    self error: 'value not found'.

- keyAt: index
    "May be overridden by subclasses so that fixCollisions will work"
    | assn |
    assn := array at: index.
    assn == nil ifTrue: [^ nil]
          ifFalse: [^ assn key].

- noCheckAdd: anObject
    "Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

    array at: (self findElementOrNil: anObject key) put: anObject.
    tally := tally + 1.

- rehash
    "Smalltalk rehash."
    | newSelf |
    newSelf := self species new: self size.
    self associationsDo: [:each | newSelf noCheckAdd: each].
    array := newSelf array.

- scanFor: anObject
    "Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches
    anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various
    subclasses that have different interpretations for matching elements."
    | element start finish |
    start := (anObject hash \\ array size) + 1.
    finish := array size.

    "Search from (hash mod size) to the end."
    start to: finish do:
      [:index | ((element := array at: index) == nil or: [element key = anObject])
        ifTrue: [^ index ]].

    "Search from 1 to where we started."
    1 to: start-1 do:
      [:index | ((element := array at: index) == nil or: [element key = anObject])
        ifTrue: [^ index ]].

    ^ 0.

- valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary
    "Support for coordinating class variable and global declarations
    with variables that have been put in Undeclared so as to
    redirect all references to the undeclared variable."

    (aDictionary includesKey: aKey)
      ifTrue:
        [self atNewIndex: index
          put: ((aDictionary associationAt: aKey) value: anObject).
        aDictionary removeKey: aKey]
      ifFalse:
        [self atNewIndex: index put: (Association key: aKey value: anObject)].


"Dictionary methodsFor: 'user interface'.

- inspect
    "Open a DictionaryInspector on the receiver.  Use basicInspect to get a normal (less useful) type of inspector."

    DictionaryInspector openOn: self withEvalPane: true.

- inspectFormsWithLabel: aLabel
    "Open a Form Dictionary inspector on the receiver, with the given label.  "

    ^ DictionaryInspector openOn: self withEvalPane: true
      withLabel: aLabel
      valueViewClass: FormInspectView.

- inspectWithLabel: aLabel
    "Open a DictionaryInspector on the receiver. Use basicInspect to get a normal (less useful) type of inspector."

    DictionaryInspector openOn: self withEvalPane: true withLabel: aLabel.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Dictionary class methodsFor: 'instance creation'"

+ fromBraceStack: itsSize
    "Answer an instance of me with itsSize elements, popped in reverse order from
     the stack of thisContext sender.  Do not call directly: this is called by {1. 2. 3}
     constructs."

    ^ self newFrom: ((Array new: itsSize) fill: itsSize fromStack: thisContext sender).

+ newFrom: aDict
    "Answer an instance of me containing the same associations as aDict.
     Error if any key appears twice."
    | newDictionary |
    newDictionary := self new: aDict size.
    aDict associationsDo:
      [:x |
      (newDictionary includesKey: x key)
        ifTrue: [self error: 'Duplicate key: ' + x key printString]
        ifFalse: [newDictionary add: x]].
    ^ newDictionary.
