"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Collection subclass: #Dictionary.

"category: accessing"

- at: key ifAbsent: aBlock
    "Answer the value named by the argument, key. If key is not found, answer the result of evaluating aBlock."
    self notImplementedYet.

- associationAt: key
    "Answer the association named by the argument, key. If key is not found, report an error."
    self notImplementedYet.

- associationAt: key ifAbsent: aBlock
    "Answer the association named by the argument, key. If key is not found, answer the result of evaluating aBlock."
    self notImplementedYet.

- keyAtValue: value
    "Answer the name for the argument, value. If there is no such value, answer nil. Since values are not necessarily
     unique, answer the name for the first one encountered in the search."
    self notImplementedYet.

- keyAtValue: value ifAbsent: exceptionBlock
    "Answer the key for the argument, value. If there is no such value, answer the result of evaluating exceptionBlock."
    self notImplementedYet.

- keys
    "Answer a Set containing the receiver's keys."
    self notImplementedYet.

- values
    "Answer a Bag containing the receiver's values (includes any duplications)."
    self notImplementedYet.

"category: dictionary testing"

- includesAssociation: anAssociation
    "Answer whether the receiver has an element (association between a key and a value) that is equal to the argument,
     anAssociation."
    self notImplementedYet.

- includesKey: key
    "Answer whether the receiver has a key equal to the argument, key."
    self notImplementedYet.

"category: dictionary removing"

- removeAssociation: anAssociation
    "Remove the key and value association, anAssociation, from the receiver. Answer anAssociation."
    self notImplementedYet.

- removeKey: key
    "Remove key (and its associated value) from the receiver. If key is not in the receiver, report an error. Otherwise
     answer the value associated with key."
    self notImplementedYet.

- removeKey: key ifAbsent: aBlock
    "Remove key (and its associated value) from the receiver. If key is not in the receiver, answer the result of
     evaluating aBlock. Otherwise, answer the value named by key."
    self notImplementedYet.

"category: dictionary enumerating"

- associationsDo: aBlock
    "Evaluate aBlock for each of the receiver's key/value associations."
    self notImplementedYet.

- keysDo: aBlock
    "Evaluate aBlock for each of the receiver's keys."
    self notImplementedYet.
