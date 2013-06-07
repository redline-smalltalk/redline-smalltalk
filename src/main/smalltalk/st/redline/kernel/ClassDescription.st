"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Behavior subclass: #ClassDescription.

"category: accessing class description"

- name
   "Answer a String that is the name of the receiver."
    self notImplementedYet.

- comment
    "Answer a String that is the comment for the receiver."
    self notImplementedYet.

- comment: aString
    "Set the receiver's comment to be the argument, aString."
    self notImplementedYet.

- addInstVarName: aString
    "Add the argument, aString, as one of the receiver's instance variables."
    self notImplementedYet.

- removeInstVarName: aString
    "Remove the argument, aString, as one of the receiver's instance variables. Report an error if aString is not
     found."
    self notImplementedYet.

"category: organization of messages and classes"

- category
    "Answer the system organization category for the receiver."
    self notImplementedYet.

- category: aString
    "Categorize the receiver under the system category, aString, removing the receiver from any previous category."
    self notImplementedYet.

- removeCategory: aString
    "Remove each of the messages categorized under the name aString and then remove the category itself."
    self notImplementedYet.

- whichCategorylncludesSelector: selector
    "Answer the category of the argument, selector, in the organization of the receiver's method dictionary, or answer
     nil if the selector can not be found."
    self notImplementedYet.

"category: filing"

- fileOutOn: aFileStream
    "Store a description of the receiver on the file accessed by the argument, aFileStream."
    self notImplementedYet.

- fileOutCategory: categoryName
    "Create a file whose name is the name of the receiver concatenated by an extension, '.st'. Store on it a description
     of the messages categorized as categoryName."
    self notImplementedYet.

- fileOutChangedMessages: setOfChanges on: aFileStream
    "The argument, setOfChanges, is a collection of class/message pairs that were changed. Store a description of each
     of these pairs on the file accessed by the argument, aFileStream."
    self notImplementedYet.
