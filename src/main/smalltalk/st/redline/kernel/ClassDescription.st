"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Behavior subclass: #ClassDescription
	instanceVariableNames: 'instanceVariables organization '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'.

" TODO.JCL -
  acceptsLoggingOfCompilation
  browseClassVarRefs
  browseClassVariables
  browseInstVarDefs
  browseInstVarRefs
  chooseInstVarThenDo: aBlock
  classComment: aString
  category
  category: cat
  comment
  comment:
  comment: aStringOrText stamp: aStamp
  commentStamp: changeStamp
  commentStamp: changeStamp prior: indexAndOffset
  compile: text classified: category notifying: requestor
  compile: text classified: category withStamp: changeStamp notifying: requestor
  compile: code notifying: requestor trailer: bytes ifFail: failBlock elseSetSelectorAndNode: selAndNodeBlock
  definition
  errorCategoryName
  fileOutCategory: catName
  fileOutCategory: catName asHtml: useHtml
  fileOutCategory: aString on: aFileStream moveSource: moveSource toFile: fileIndex
  fileOutChangedMessages: aSet on: aFileStream
  fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex
  fileOutMethod: selector
  fileOutMethod: selector asHtml: useHtml
  fileOutOn: aFileStream
  fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
  fileOutOrganizationOn: aFileStream
  instanceVariablesString
  kindOfSubclass
  methodsFor: categoryName
  methodsFor: categoryName stamp: changeStamp
  methodsFor: categoryName stamp: changeStamp prior: indexAndOffset
  moveChangesTo: newFile
  obsolete
  organization
  printCategoryChunk: categoryName on: aFileStream
  printCategoryChunk: category on: aFileStream priorMethod: priorMethod
  printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
  printCategoryChunk: categoryName withStamp: changeStamp on: aFileStream
  printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
  removeCategory: aString
  removeSelector: aSymbol
  reorganize
  sharedPoolsString
  subclassOf: newSuper oldClass: oldClass instanceVariableNames: newInstVarString variable: v words: w pointers: p ifBad: badBlock
  updateInstancesFrom: oldClass
  validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods wasPresent: wasPresent
  wantsChangeSetLogging
  whichCategoryIncludesSelector: aSelector
  zapOrganization
  zapTrivialOrganization
"

- classVariablesString
	| aStream |
	aStream := WriteStream on: (String new: 100).
	self classPool keys asSortedCollection do: [:key | aStream nextPutAll: key;
		 space].
	^ aStream contents.

- isMeta
	^ false.

- name
	self subclassResponsibility.

- printOn: aStream
	aStream nextPutAll: self name.

- storeOn: aStream
	aStream nextPutAll: self name.

- theNonMetaClass
	^ self.
