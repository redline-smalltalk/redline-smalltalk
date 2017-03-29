"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I add a number of facilities to basic Behavior:
 Named instance variables
 Category organization for methods
 The notion of a name of this class (implemented as subclass responsibility)
 The maintenance of a ChangeSet, and logging changes on a file
 Most of the mechanism for fileOut.

 I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.
"

Behavior subclass: #ClassDescription
         instanceVariableNames: 'instanceVariables organization '
         category: 'Kernel-Classes'.

"ClassDescription methodsFor: 'initialize-release'"

- obsolete
    "Make the receiver obsolete."

    organization := nil.
    super obsolete.

- subclassOf: newSuper oldClass: oldClass instanceVariableNames: newInstVarString variable: v words: w pointers: p ifBad: badBlock
    "Basic initialization message for creating classes using the information
    provided as arguments. Answer whether old instances will be
    invalidated."

    | oldNames newNames usedNames invalid oldSuperMeta newInstVarArray oldSpec |
    oldNames := self allInstVarNames.
    usedNames := #(self super thisContext true false nil ) asSet.
    newInstVarArray := Scanner new scanFieldNames: newInstVarString.
    newNames := newSuper allInstVarNames + newInstVarArray.
    newNames size > 254 ifTrue:
      [self error: 'A class cannot have more than 254 instance variables'.
      ^ badBlock value].
    newNames do:
      [:fieldName |
      (usedNames includes: fieldName)
        ifTrue:
          [self error: fieldName + ' is reserved (maybe in a superclass)'.
          ^ badBlock value].
      usedNames add: fieldName].
    (invalid := superclass ~~ newSuper)
      ifTrue:
        ["superclass changed"
        oldSuperMeta := superclass class.
        superclass ifNotNil: [superclass removeSubclass: self.
          "Object flushCache"		"done in removeSubclass"].
        superclass := newSuper.
        superclass addSubclass: self.
        self class superclass == oldSuperMeta
          ifTrue: ["Only false when self is a metaclass"
              self class superclass: newSuper class]].
    instanceVariables := newInstVarArray size = 0 ifFalse: [newInstVarArray].
    invalid := invalid | (newNames  ~= oldNames).   "field names changed"
    oldSpec := self instSpec.
    self format: newNames size
      variable: v
      words: w
      pointers: p.
    invalid := invalid | (self instSpec ~= oldSpec).  "format changed"
    ^ invalid.

- updateInstancesFrom: oldClass
    "Recreate any existing instances of the argument, oldClass, as instances of
    the receiver, which is a newly changed class. Permute variables as
    necessary."

    | oldInstVarNames map variable new instSize oldInstances |
    oldClass someInstance == nil ifTrue: [^ self].
    "no instances to convert"
    oldInstVarNames := oldClass allInstVarNames.
    map :=
      self allInstVarNames
        collect: [:instVarName | oldInstVarNames indexOf: instVarName].
    variable := self isVariable.
    instSize := self instSize.

    "Now perform a bulk mutation of old instances into new ones"
    oldInstances := oldClass allInstances asArray.
    oldInstances elementsExchangeIdentityWith:
      (oldInstances collect:
      [:old |
      variable
        ifTrue: [new := self basicNew: old basicSize]
        ifFalse: [new := self basicNew].
      1 to: instSize do:
        [:offset |  (map at: offset) > 0 ifTrue:
          [new instVarAt: offset
              put: (old instVarAt: (map at: offset))]].
      variable
        ifTrue: [1 to: old basicSize do:
              [:offset |
              new basicAt: offset put: (old basicAt: offset)]].
      new]).

- validateFrom: oldClass in: environ instanceVariableNames: invalidFields methods: invalidMethods
    "Recompile the receiver, a class, and redefine its subclasses if necessary.
    The parameter invalidFields is no longer really used"

    | newSub invalidSubMethods |
    oldClass becomeUncompact.  "Its about to be abandoned"
    invalidMethods & self hasMethods
      ifTrue:
        [Transcript show: 'recompiling ' + self name + '...'.
        self compileAllFrom: oldClass.
        Transcript show: ' done'; cr].
    invalidSubMethods := invalidMethods | (self instSize ~= oldClass instSize).
    self == oldClass
      ifTrue: [invalidSubMethods ifFalse: [^ self]]
      ifFalse: [self updateInstancesFrom: oldClass].
    oldClass subclasses do:
      [:sub |
      newSub := sub copyForValidation.
      newSub
        subclassOf: self
        oldClass: sub
        instanceVariableNames: sub instVarNames
        variable: sub isVariable
        words: sub isBytes not
        pointers: sub isBits not
        ifBad: [self error: 'terrible problem in recompiling subclasses!!'].
      newSub
        validateFrom: sub
        in: environ
        instanceVariableNames: invalidFields
        methods: invalidSubMethods].

"ClassDescription methodsFor: 'accessing'"

- classVersion
	"Default. Any class may return a later version to inform readers that use ReferenceStream."
	^ 0.

"ClassDescription methodsFor: 'accessing'"

- comment
    "Answer the receiver's comment. (If old format, not a Text, unpack the old way.) "

    | aString |
    aString := self theNonMetaClass organization classComment.
    (aString asString beginsWith: self name + ' comment:\''' withCRs)
      ifFalse: [^ self theNonMetaClass organization classComment]
      ifTrue: ["old format"
        aString size = 0 ifTrue: [^ ''].
        "get string only of classComment, undoubling quotes"
        ^ String readFromString: aString].

- comment: aStringOrText
    "Set the receiver's comment to be the argument, aStringOrText."

    self theNonMetaClass classComment: aStringOrText.
    Smalltalk changes commentClass: self.

- isMeta
	^ false.

- name
    "Answer a String that is the name of the receiver."

    self subclassResponsibility.

- theNonMetaClass
    "Sent to a class or metaclass, always return the class"

    ^ self.


"ClassDescription methodsFor: 'copying'"

- copy: sel from: class
    "Install the method associated with the first argument, sel, a message
    selector, found in the method dictionary of the second argument, class,
    as one of the receiver's methods. Classify the message under -As yet not
    classified-."

    self copy: sel
      from: class
      classified: nil.

- copy: sel from: class classified: cat
    "Install the method associated with the first arugment, sel, a message
    selector, found in the method dictionary of the second argument, class,
    as one of the receiver's methods. Classify the message under the third
    argument, cat."

    | code category |
    "Useful when modifying an existing class"
    code := class sourceMethodAt: sel.
    code == nil
      ifFalse:
        [cat == nil
          ifTrue: [category := class organization categoryOfElement: sel]
          ifFalse: [category := cat].
        (methodDict includesKey: sel)
          ifTrue: [code asString = (self sourceMethodAt: sel) asString
                ifFalse: [self error: self name
                      + ' '
                      + sel
                      + ' will be redefined if you proceed.']].
        self compile: code classified: category].

- copyAll: selArray from: class
    "Install all the methods found in the method dictionary of the second
    argument, class, as the receiver's methods. Classify the messages under
    -As yet not classified-."

    self copyAll: selArray
      from: class
      classified: nil.

- copyAll: selArray from: class classified: cat
    "Install all the methods found in the method dictionary of the second
    argument, class, as the receiver's methods. Classify the messages under
    the third argument, cat."

    selArray do:
      [:s | self copy: s
          from: class
          classified: cat].

- copyAllCategoriesFrom: aClass
    "Specify that the categories of messages for the receiver include all of
    those found in the class, aClass. Install each of the messages found in
    these categories into the method dictionary of the receiver, classified
    under the appropriate categories."

    aClass organization categories do: [:cat | self copyCategory: cat from: aClass].

- copyCategory: cat from: class
    "Specify that one of the categories of messages for the receiver is cat, as
    found in the class, class. Copy each message found in this category."

    self copyCategory: cat
      from: class
      classified: cat.

- copyCategory: cat from: aClass classified: newCat
    "Specify that one of the categories of messages for the receiver is the
    third argument, newCat. Copy each message found in the category cat in
    class aClass into this new category."

    self copyAll: (aClass organization listAtCategoryNamed: cat)
      from: aClass
      classified: newCat.


"ClassDescription methodsFor: 'printing'"

- classVariablesString
    "Answer a string of my class variable names separated by spaces."
    | aStream |
    aStream := WriteStream on: (String new: 100).
    self classPool keys asSortedCollection do: [:key | aStream nextPutAll: key; space].
    ^ aStream contents.

- instanceVariablesString
    "Answer a string of my instance variable names separated by spaces."

    | aStream names |
    aStream := WriteStream on: (String new: 100).
    names := self instVarNames.
    1 to: names size do: [:i | aStream nextPutAll: (names at: i); space].
    ^ aStream contents.

- printOn: aStream

    aStream nextPutAll: self name.

- sharedPoolsString
    "Answer a string of my shared pool names separated by spaces."

    | aStream |
    aStream := WriteStream on: (String new: 100).
    self sharedPools do: [:x | aStream nextPutAll: (Smalltalk keyAtValue: x ifAbsent: ['private']); space].
    ^ aStream contents.

- storeOn: aStream
    "Classes and Metaclasses have global names."

    aStream nextPutAll: self name.


"ClassDescription methodsFor: 'instance variables'"

- addInstVarName: aString
    "Add the argument, aString, as one of the receiver's instance variables."

    self subclassResponsibility.

- browseClassVariables
    "Put up a browser showing the receiver's class variables."

    self classPool inspectWithLabel: 'Class Variables in ' + self name.

- browseClassVarRefs
    "1/17/96 sw: moved here from Browser so that it could be used from a variety of places."

    | lines labelStream vars allVars index owningClasses |

    lines := OrderedCollection new.
    allVars := OrderedCollection new.
    owningClasses := OrderedCollection new.
    labelStream _ WriteStream on: (String new: 200).
    self withAllSuperclasses reverseDo:
      [:class |
      vars := class classVarNames asSortedCollection.
      vars do:
        [:var |
        labelStream nextPutAll: var; cr.
        allVars add: var.
        owningClasses add: class].
      vars isEmpty ifFalse: [lines add: allVars size]].
    labelStream skip: -1 "cut last CR".
    index := (PopUpMenu labels: labelStream contents lines: lines) startUp.
    index = 0 ifTrue: [^ self].
    Smalltalk browseAllCallsOn:
      ((owningClasses at: index) classPool associationAt: (allVars at: index)).

- browseInstVarDefs
    "Copied from browseInstVarRefs."

    self chooseInstVarThenDo:
      [:aVar | self browseAllStoresInto: aVar].

- browseInstVarRefs

	self chooseInstVarThenDo:
		[:aVar | self browseAllAccessesTo: aVar].

- chooseInstVarThenDo: aBlock
    "Put up a menu of all the instance variables in the receiver, and when the user chooses one, evaluate aBlock with
    the chosen variable as its parameter."

    | lines labelStream vars allVars index |

    lines := OrderedCollection new.
    allVars := OrderedCollection new.
    labelStream := WriteStream on: (String new: 200).
    self withAllSuperclasses reverseDo:
      [:class |
      vars := class instVarNames.
      vars do:
        [:var |
        labelStream nextPutAll: var; cr.
        allVars add: var].
      vars isEmpty ifFalse: [lines add: allVars size]].
    labelStream isEmpty ifTrue:
      [^ (PopUpMenu labels: ' OK ')
        startUpWithCaption: 'There are no
  instance variables.'].
    labelStream skip: -1 "cut last CR".
    index := (PopUpMenu labels: labelStream contents lines: lines) startUpWithCaption: 'Instance variables in
  ' + class name.
    index = 0 ifTrue: [^ self].
    aBlock value: (allVars at: index).

- forceNewFrom: anArray
      "Create a new instance of the class and fill
      its instance variables up with the array."
      | object max |

      object := self new.
      max := self instSize.
      anArray doWithIndex: [:each :index |
          index > max ifFalse:
              [object instVarAt: index put: each]].
      ^ object.

- instVarNames
    "Answer an Array of the receiver's instance variable names."

    instanceVariables == nil
      ifTrue: [^ #()]
      ifFalse: [^ instanceVariables].

- removeInstVarName: aString
    "Remove the argument, aString, as one of the receiver's instance
    variables. Create an error notification if the argument is not found."

    self subclassResponsibility.

- renameInstVar: oldName to: newName
    | i oldCode newCode parser header body sels |
    (i := instanceVariables indexOf: oldName) = 0 ifTrue:
      [self error: oldName + ' is not defined in ' + self name].
    self allSuperclasses + self withAllSubclasses asOrderedCollection do:
      [:cls | (cls instVarNames includes: newName) ifTrue:
        [self error: newName + ' is already used in ' + cls name]].
    (self confirm: 'WARNING: Renaming of instance variables
  is subject to substitution ambiguities.
  Do you still wish to attempt it?') ifFalse: [self halt].

    "...In other words, this does a dumb text search-and-replace,
    which might improperly alter, eg, a literal string.  As long as
    the oldName is unique, everything should work jes' fine. - di"
    instanceVariables replaceFrom: i to: i with: (Array with: newName).
    self withAllSubclasses do:
      [:cls | sels := cls selectors.
      sels removeAllFoundIn: #(DoIt DoItIn:).
      sels do:
        [:sel |
        oldCode := cls sourceCodeAt: sel.
        "Don't make changes in the method header"
        (parser := cls parserClass new) parseSelector: oldCode.
        header := oldCode copyFrom: 1 to: (parser endOfLastToken min: oldCode size).
        body := header size > oldCode size
            ifTrue: ['']
            ifFalse: [oldCode copyFrom: header size+1 to: oldCode size].
        newCode := header + (body copyReplaceTokens: oldName with: newName).
        newCode ~= oldCode ifTrue:
          [cls compile: newCode
            classified: (cls organization categoryOfElement: sel)
            notifying: nil]].
        cls isMeta ifFalse:
          [oldCode := cls comment.
          newCode := oldCode copyReplaceTokens: oldName with: newName.
          newCode ~= oldCode ifTrue:
            [cls comment: newCode]]].


"ClassDescription methodsFor: 'method dictionary'"

- removeCategory: aString
    "Remove each of the messages categorized under aString in the method
    dictionary of the receiver. Then remove the category aString."
    | categoryName |
    categoryName := aString asSymbol.
    (self organization listAtCategoryNamed: categoryName) do:
      [:sel | self removeSelector: sel].
    self organization removeCategory: categoryName.

- removeSelector: aSymbol
    "Remove the message whose selector is aSymbol from the method
    dictionary of the receiver, if it is there. Answer nil otherwise."

    (methodDict includesKey: aSymbol) ifFalse: [^ nil].
    self wantsChangeSetLogging ifTrue:
      [Smalltalk changes removeSelector: aSymbol class: self].
    super removeSelector: aSymbol.
    self organization removeElement: aSymbol.
    self acceptsLoggingOfCompilation ifTrue:
      [Smalltalk logChange: self name + ' removeSelector: #' + aSymbol].


"ClassDescription methodsFor: 'organization'"

- category
    "Answer the system organization category for the receiver."

    ^ SystemOrganization categoryOfElement: self name.

- category: cat
    "Categorize the receiver under the system category, cat, removing it from
    any previous categorization."

    (cat isKindOf: String)
      ifTrue: [SystemOrganization classify: self name under: cat asSymbol]
      ifFalse: [self errorCategoryName].

- organization
    "Answer the instance of ClassOrganizer that represents the organization
    of the messages of the receiver."

    organization==nil
      ifTrue: [organization _
           ClassOrganizer defaultList:
              methodDict keys asSortedCollection asArray].
    ^ organization.

- whichCategoryIncludesSelector: aSelector
    "Answer the category of the argument, aSelector, in the organization of
    the receiver, or answer nil if the receiver does not inlcude this selector."

    (self includesSelector: aSelector)
      ifTrue: [^ self organization categoryOfElement: aSelector]
      ifFalse: [^ nil].

- zapOrganization
    "Remove the organization of this class by message categories.
    This is typically done to save space in small systems.  Classes and methods
    created or filed in subsequently will, nonetheless, be organized"

    organization := nil.
    self isMeta ifFalse: [self class zapOrganization].


"ClassDescription methodsFor: 'compiling'"

- acceptsLoggingOfCompilation
    "weird name is so that it will come lexically before #compile, so that a clean build can make it through."

    ^ true.

- compile: code classified: heading
    "Compile the argument, code, as source code in the context of the
    receiver and install the result in the receiver's method dictionary under
    the classification indicated by the second argument, heading. nil is to be
    notified if an error occurs. The argument code is either a string or an
    object that converts to a string or a PositionableStream on an object that
    converts to a string."

    ^ self
      compile: code
      classified: heading
      notifying: (SyntaxError new category: heading).

- compile: text classified: category notifying: requestor
    | stamp |
    stamp := self acceptsLoggingOfCompilation ifTrue: [Utilities changeStamp] ifFalse: [nil].
    ^ self compile: text classified: category
      withStamp: stamp notifying: requestor.

- compile: text classified: category withStamp: changeStamp notifying: requestor
    | selector priorMethod method methodNode newText |
    method := self
      compile: text asString
      notifying: requestor
      trailer: #(0 0 0 0)
      ifFail: [^ nil]
      elseSetSelectorAndNode:
        [:sel :node | selector := sel.
        priorMethod := methodDict at: selector ifAbsent: [nil].
        methodNode := node].
    self acceptsLoggingOfCompilation ifTrue:
      [newText := (requestor ~~ nil and: [Preferences confirmFirstUseOfStyle])
        ifTrue: [text askIfAddStyle: priorMethod req: requestor]
        ifFalse: [text].
       method putSource: newText
          fromParseNode: methodNode
          class: self category: category withStamp: changeStamp
          inFile: 2 priorMethod: priorMethod].
    self organization classify: selector under: category.
    ^ selector.

- compile: code notifying: requestor
    "Refer to the comment in Behavior|compile:notifying:."

    ^ self compile: code
       classified: ClassOrganizer default
       notifying: requestor.

- compile: code notifying: requestor trailer: bytes ifFail: failBlock elseSetSelectorAndNode: selAndNodeBlock
	"Intercept this message in order to remember system changes.
	 modified so that if the class does not wish its methods logged in the changes file, then they also won't be accumulated in the current change set.
	 use wantsChangeSetLogging to determine whether to put in change set"

	| methodNode selector method |
	methodNode := self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	selector := methodNode selector.
	selAndNodeBlock value: selector value: methodNode.
	self wantsChangeSetLogging ifTrue:
		[(methodDict includesKey: selector)
			ifTrue: [Smalltalk changes changeSelector: selector class: self]
			ifFalse: [Smalltalk changes addSelector: selector class: self]].
	methodNode encoder requestor: requestor.  "Why was this not preserved?"
	method := methodNode generate: bytes.
	self addSelector: selector withMethod: method.
	^ method.

- wantsChangeSetLogging
    "Answer whether code submitted for the receiver should be remembered by the changeSet mechanism."

    ^ true.

"ClassDescription methodsFor: 'fileIn/Out' stamp: 'tk 12/29/97 13:11'"

- classComment: aString
    "Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."

    | ptr header file oldCommentRemoteStr |
    (aString isKindOf: RemoteString) ifTrue: [^ organization classComment: aString].
    oldCommentRemoteStr := organization commentRemoteStr.
    (aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ organization classComment: nil].
      "never had a class comment, no need to write empty string out"

    ptr := oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
    SourceFiles ifNotNil: [(file := SourceFiles at: 2) ifNotNil: [
      file setToEnd; cr; nextPut: $!!.	"directly"
      "Should be saying (file command: 'H3') for HTML, but ignoring it here"
      header := String streamContents: [:strm | strm nextPutAll: self name;
        nextPutAll: ' commentStamp: '.
        Utilities changeStamp storeOn: strm.
        strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
      file nextChunkPut: header]].
    organization classComment: (RemoteString newString: aString onFileNumber: 2).
    Smalltalk changes commentClass: self.

- commentFollows
    "Answer a ClassCommentReader who will scan in the comment."

    ^ ClassCommentReader new setClass: self category: #Comment.

- commentStamp: changeStamp prior: indexAndOffset
    "Prior source link ignored when filing in."

    ^ ClassCommentReader new setClass: self
          category: #Comment
          changeStamp: changeStamp

- definition
    "Answer a String that defines the receiver."

    | aStream |
    aStream := WriteStream on: (String new: 300).
    aStream nextPutAll:
      (superclass == nil
        ifTrue: ['nil']
        ifFalse: [superclass name])
      , self kindOfSubclass.
    self name storeOn: aStream.
    aStream cr; tab; nextPutAll: 'instanceVariableNames: '.
    aStream store: self instanceVariablesString.
    aStream cr; tab; nextPutAll: 'classVariableNames: '.
    aStream store: self classVariablesString.
    aStream cr; tab; nextPutAll: 'poolDictionaries: '.
    aStream store: self sharedPoolsString.
    aStream cr; tab; nextPutAll: 'category: '.
    (SystemOrganization categoryOfElement: self name) asString storeOn: aStream.
    ^ aStream contents.

- fileOutCategory: catName
  	^ self fileOutCategory: catName asHtml: false.

- fileOutCategory: catName asHtml: useHtml
    "FileOut the named category, possibly in Html format."
    | fileStream |
    fileStream := useHtml
      ifTrue: [(FileStream newFileNamed: self name + '-' + catName + '.html') asHtml]
      ifFalse: [FileStream newFileNamed: self name + '-' + catName + '.st'].
    fileStream header; timeStamp.
    self fileOutCategory: catName on: fileStream moveSource: false toFile: 0.
    fileStream trailer; close.

- fileOutCategory: aString on: aFileStream moveSource: moveSource toFile: fileIndex
    "File a description of the receiver's category, aString, onto aFileStream. If
    moveSource, is true, then set the method source pointer to the new file position.
    Note when this method is called with moveSource=true, it is condensing the
    .sources file, and should only write one preamble per method category."

    aFileStream cr.

    true ifTrue:
      ["Overridden to preserve author stamps in sources file regardless"
      (self organization listAtCategoryNamed: aString)
        do: [:sel | self printMethodChunk: sel withPreamble: true
                on: aFileStream moveSource: moveSource toFile: fileIndex].
      ^ self].

    moveSource ifTrue:
      ["Single header for condensing source files"
      self printCategoryChunk: aString on: aFileStream].
    (self organization listAtCategoryNamed: aString)
      do: [:sel | self printMethodChunk: sel withPreamble: moveSource not
              on: aFileStream moveSource: moveSource toFile: fileIndex].
    moveSource ifTrue: [aFileStream nextChunkPut: ' '].


- fileOutChangedMessages: aSet on: aFileStream
    "File a description of the messages of the receiver that have been
    changed (i.e., are entered into the argument, aSet) onto aFileStream."

    self fileOutChangedMessages: aSet
      on: aFileStream
      moveSource: false
      toFile: 0.

- fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex
    "File a description of the messages of this class that have been
    changed (i.e., are entered into the argument, aSet) onto aFileStream.  If
    moveSource, is true, then set the method source pointer to the new file position.
    Note when this method is called with moveSource=true, it is condensing the
    .changes file, and should only write a preamble for every method."
    | org sels |
    (org := self organization) categories do:
      [:cat |
      sels := (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
      sels do:
        [:sel |  self printMethodChunk: sel withPreamble: true on: aFileStream
                moveSource: moveSource toFile: fileIndex]].

- fileOutMethod: selector
    "Write source code of a single method on a file.  Make up a name for the file."
    self fileOutMethod: selector asHtml: false.

- fileOutMethod: selector asHtml: useHtml
    "Write source code of a single method on a file in .st or .html format"
    | fileStream nameBody |
    (self includesSelector: selector) ifFalse: [^ self halt: 'Selector not found'].
    nameBody := self name + '-' + (selector copyReplaceAll: ':' with: '').
    fileStream := useHtml
      ifTrue: [(FileStream newFileNamed: nameBody + '.html') asHtml]
      ifFalse: [FileStream newFileNamed: nameBody + '.st'].
    fileStream header; timeStamp.
    self printMethodChunk: selector withPreamble: true
      on: fileStream moveSource: false toFile: 0.
    fileStream close.

- fileOutOn: aFileStream
    "File a description of the receiver on aFileStream."

    self fileOutOn: aFileStream
      moveSource: false
      toFile: 0.

- fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex
    "File a description of the receiver on aFileStream. If the boolean
    argument, moveSource, is true, then set the trailing bytes to the position
    of aFileStream and to fileIndex in order to indicate where to find the
    source code."

    aFileStream command: 'H3'.
      aFileStream nextChunkPut: self definition.
      aFileStream command: '/H3'.

    self organization
      putCommentOnFile: aFileStream
      numbered: fileIndex
      moveSource: moveSource
      forClass: self.
    self organization categories do:
      [:heading |
      self fileOutCategory: heading
        on: aFileStream
        moveSource: moveSource
        toFile: fileIndex].

- fileOutOrganizationOn: aFileStream
    "File a description of the receiver's organization on aFileStream."

    aFileStream cr; nextPut: $!!.
    aFileStream nextChunkPut: self name + ' reorganize'; cr.
    aFileStream nextChunkPut: self organization printString; cr.

- kindOfSubclass
    "Answer a string that describes what kind of subclass the receiver is, i.e.,
    variable, variable byte, variable word, or not variable."

    self isVariable
      ifTrue: [self isBits
            ifTrue: [self isBytes
                  ifTrue: [^ ' variableByteSubclass: ']
                  ifFalse: [^ ' variableWordSubclass: ']]
            ifFalse: [^ ' variableSubclass: ']]
      ifFalse: [^ ' subclass: '].

- methods
    "Answer a ClassCategoryReader for compiling messages that are not classified, as in fileouts made with Smalltalk/V"

    ^ ClassCategoryReader new setClass: self
                category: 'as yet unclassified' asSymbol.

- methodsFor: categoryName
    "Answer a ClassCategoryReader for compiling the messages in the category, categoryName, of the receiver."

    ^ ClassCategoryReader new setClass: self category: categoryName asSymbol.

- methodsFor: aString priorSource: sourcePosition inFile: fileIndex
    "Prior source pointer ignored when filing in."
    ^ self methodsFor: aString.

- methodsFor: categoryName stamp: changeStamp
  	^ self methodsFor: categoryName stamp: (Utilities fixStamp: changeStamp) prior: 0.

- methodsFor: categoryName stamp: changeStamp prior: indexAndOffset
    "Prior source link ignored when filing in."
    ^ ClassCategoryReader new setClass: self
          category: categoryName asSymbol
          changeStamp: changeStamp.

- moveChangesTo: newFile
    "Used in the process of condensing changes, this message requests that
    the source code of all methods of the receiver that have been changed
    should be moved to newFile."

    | changes |
    self organization moveChangedCommentToFile: newFile numbered: 2.
    changes := methodDict keys select: [:sel | (methodDict at: sel) fileIndex > 1].
    self fileOutChangedMessages: changes
      on: newFile
      moveSource: true
      toFile: 2.

- printCategoryChunk: categoryName on: aFileStream
  	^ self printCategoryChunk: categoryName withStamp: '' on: aFileStream.

- printCategoryChunk: category on: aFileStream priorMethod: priorMethod
    ^ self printCategoryChunk: category on: aFileStream
      withStamp: Utilities changeStamp priorMethod: priorMethod.

- printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
    "Print a method category preamble.  This must have a category name.
    It may have an author/date stamp, and it may have a prior source link.
    If it has a prior source link, it MUST have a stamp, even if it is empty."

  "The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

    aFileStream cr; command: 'H3'; nextPut: $!!.
    aFileStream nextChunkPut: (String streamContents:
      [:strm |
      strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
      (changeStamp size > 0 or: [priorMethod ~~ nil]) ifTrue:
        [strm nextPutAll: ' stamp: '; print: changeStamp].
      priorMethod ~~ nil ifTrue:
        [strm nextPutAll: ' prior: '; print: priorMethod sourcePointer]]).
    aFileStream command: '/H3'.

- printCategoryChunk: categoryName withStamp: changeStamp on: aFileStream
    ^ self printCategoryChunk: categoryName on: aFileStream withStamp: changeStamp
      priorMethod: nil.

- printMethodChunk: selector withPreamble: doPreamble on: outStream	moveSource: moveSource toFile: fileIndex
    "Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
    | preamble method oldPos newPos sourceFile |
    doPreamble
      ifTrue: [preamble := self name + ' methodsFor: ' +
            (self organization categoryOfElement: selector) asString printString]
      ifFalse: [preamble := ''].
    method := methodDict at: selector.
    ((method fileIndex = 0
      or: [(SourceFiles at: method fileIndex) == nil])
      or: [(oldPos := method filePosition) = 0])
      ifTrue:
      ["The source code is not accessible.  We must decompile..."
      preamble size > 0 ifTrue: [outStream cr; nextPut: $!!; nextChunkPut: preamble; cr].
      outStream nextChunkPut: (self decompilerClass new decompile: selector
                        in: self method: method) decompileString]
      ifFalse:
      [sourceFile := SourceFiles at: method fileIndex.
      sourceFile position: oldPos.
      preamble size > 0 ifTrue:    "Copy the preamble"
        [outStream copyPreamble: preamble from: sourceFile].
      "Copy the method chunk"
      newPos := outStream position.
      outStream copyMethodChunkFrom: sourceFile.
      sourceFile skipSeparators.	"The following chunk may have ]style["
      sourceFile peek == $] ifTrue: [
        outStream cr; copyMethodChunkFrom: sourceFile].
      moveSource ifTrue:    "Set the new method source pointer"
        [method setSourcePosition: newPos inFile: fileIndex]].
    preamble size > 0 ifTrue: [outStream nextChunkPut: ' '].
    ^ outStream cr.

- reformatAll
    "Reformat all methods in this class.
    Leaves old code accessible to version browsing"
    self selectorsDo: [:sel | self reformatMethodAt: sel].

- reformatMethodAt: selector
    | newCodeString method |
    newCodeString := (self compilerClass new)
      format: (self sourceCodeAt: selector)
      in: self
      notifying: nil.
    method := self compiledMethodAt: selector.
    method
      putSource: newCodeString
      fromParseNode: nil
      class: self
      category: (self organization categoryOfElement: selector)
      inFile: 2 priorMethod: method.

- reorganize
    "Record that the receiver is being reorganized and answer the receiver's organization."

    Smalltalk changes reorganizeClass: self.
    ^ self organization.


"ClassDescription methodsFor: 'private'"

- errorCategoryName
  	self error: 'Category name must be a String'.

- spaceUsed
    "Answer a rough estimate of number of bytes in this class and its metaclass"
    | space method |
    space := 0.
    self selectorsDo:
      [:sel | space := space + 16.  "dict and org'n space"
      method := self compiledMethodAt: sel.
      space := space + (method size + 6 "hdr + avg pad").
      method literals do:
        [:lit | ((lit isMemberOf: Symbol) or: [lit isMemberOf: SmallInteger]) ifFalse:
          [(lit isMemberOf: String) ifTrue: [space := space + (lit size+6)].
          (lit isMemberOf: Array) ifTrue: [space := space + (lit size+1*4)]]]].
    (self isMemberOf: Metaclass)
      ifTrue: [^ space]
      ifFalse: [^ space + self class spaceUsed].
