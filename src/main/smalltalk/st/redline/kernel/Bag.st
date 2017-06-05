"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only
 once, my clients should beware that objects they store will not necessarily be retrieved such that == is true.
 If the client cares, a subclass of me should be created."

Collection subclass: #Bag
           instanceVariableNames: 'contents '
           category: 'Collections-Unordered'.

"Bag methodsFor: 'accessing'"

+ at: index

    self errorNotKeyed.

- at: index put: anObject

    self errorNotKeyed.

- cumulativeCounts
    "Answer with a collection of cumulative percents covered by elements so far."

    | s n |
    s := self size // 100.0.
    n := 0.
    ^ self sortedCounts asArray collect:
      [:a | n := n + a key. (n // s roundTo: 0.1) -> a value].

- size

    | tally |
    tally := 0.
    contents do: [:each | tally := tally + each].
    ^ tally.

- sortedCounts
    "Answer with a collection of counts with elements, sorted by decreasing
    count."

    | counts |
    counts := SortedCollection sortBlock: [:x :y | x >= y].
    contents associationsDo:
      [:assn |
      counts add: (Association key: assn value value: assn key)].
    ^ counts.

- sortedElements
    "Answer with a collection of elements with counts, sorted by element."

    | elements |
    elements := SortedCollection new.
    contents associationsDo: [:assn | elements add: assn].
    ^ elements.

- includes: anObject
    "Refer to the comment in Collection|includes:."

    ^ contents includesKey: anObject.

"Bag methodsFor: 'testing'"

- occurrencesOf: anObject
    "Refer to the comment in Collection|occurrencesOf:."

    (self includes: anObject)
      ifTrue: [^ contents at: anObject]
      ifFalse: [^ 0].

"Bag methodsFor: 'adding'"

- add: newObject
    "Refer to the comment in Collection|add:."

    ^ self add: newObject withOccurrences: 1.

- add: newObject withOccurrences: anInteger
    "Add the element newObject to the receiver. Do so as though the element
    were added anInteger number of times. Answer newObject."

    (self includes: newObject)
      ifTrue: [contents at: newObject put: anInteger + (contents at: newObject)]
      ifFalse: [contents at: newObject put: anInteger].
    ^ newObject.


"Bag methodsFor: 'removing'"

- remove: oldObject ifAbsent: exceptionBlock
    "Refer to the comment in Collection|remove:ifAbsent:."

    | count |
    (self includes: oldObject)
      ifTrue: [(count := contents at: oldObject) = 1
          ifTrue: [contents removeKey: oldObject]
          ifFalse: [contents at: oldObject put: count - 1]]
      ifFalse: [^ exceptionBlock value].
    ^ oldObject.

"Bag methodsFor: 'enumerating'"

- asSet
    "Answer a set with the elements of the receiver"

     ^ contents keys.

- do: aBlock
    "Refer to the comment in Collection|do:."

    contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]].


"Bag methodsFor: 'private'"

- setDictionary

    contents := Dictionary new.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "

"Bag class methodsFor: 'instance creation'"

+ new

    ^ super new setDictionary.

+ newFrom: aCollection
    "Answer an instance of me containing the same elements as aCollection."

    | newCollection |
    newCollection := self new.
    newCollection addAll: aCollection.
    ^ newCollection.
