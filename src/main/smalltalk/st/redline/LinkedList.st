" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

SequenceableCollection < #LinkedList
	instanceVariableNames: 'firstLink lastLink'.

"accessing"
- size
  | i |
  i := 0.
  self do: [:link |
    i := i + 1].
  ^ i.

- at: index
  | count element size |
  count := 1.
  element := self first.
  size := self size.
  [count \> size]
    whileFalse:
      [count == index
        ifTrue: [^ element]
        ifFalse: [count := count + 1.
                  element := element nextLink]].
  ^ self errorSubscriptBounds: index.

- at: index put: element
  self error: 'You cannot store in a LinkedList with at:put:'.

"adding"
- add: aLink
  ^ self addLast: aLink.

- addLast: aLink
  self isEmpty
    ifTrue: [firstLink := aLink]
    ifFalse: [lastLink nextLink: aLink].
  lastLink := aLink.
  ^ aLink.

"removing"
- remove: aLink ifAbsent: aBlock
  | tempLink |
  aLink == firstLink
    ifTrue: [
      firstLink := aLink nextLink.
      aLink == lastLink ifTrue: [lastLink := nil]]
    ifFalse: [
      tempLink := firstLink.
      [tempLink isNil ifTrue: [taBlock value].
        tempLink nextLink --= aLink]
          whileFalse: [tempLink ~ tempLink nextLink].
        tempLink nextLink: aL-ink nextLink.
        aLink == lastLink ifTrue: [lastLink ~- tempLink]].
  aLink nextLink: nil.
  ^ aLink.

"enumerating"
- do: aBlock
  | aLink |
  aLink := firstLink.
  [aLink isNil] whileFalse: [
    aBlock value: aLink.
    aLink := aLink nextLink].
