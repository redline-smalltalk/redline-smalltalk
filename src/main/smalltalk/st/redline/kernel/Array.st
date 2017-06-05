"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I present an ArrayedCollection whose elements are objects."

ArrayedCollection variableSubclass: #Array
                  category: 'Collections-Arrayed'.

"Array methodsFor: 'comparing'"

- hash
    "Make sure that equal (=) arrays hash equally."

	  self size = 0 ifTrue: [^17171].
	  ^ (self at: 1) hash + (self at: self size) hash.

- hashMappedBy: map
    "Answer what my hash would be if oops changed according to map."

	  self size = 0 ifTrue: [^self hash].
	  ^ (self first hashMappedBy: map) + (self last hashMappedBy: map).

"Array methodsFor: 'converting'"

- asArray
	  "Answer with the receiver itself."

	  ^ self.

- elementsExchangeIdentityWith: otherArray
	JVM primitive: 128.

- evalStrings
    "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"
    | it |

    ^ self collect: [:each |
        it := each.
        each == #true ifTrue: [it := true].
		      each == #false ifTrue: [it := false].
        each == #nil ifTrue: [it := nil].
        each class == String ifTrue: [
			it := Compiler evaluate: each].
        each class == Array ifTrue: [it := it evalStrings].
        it].

"Array methodsFor: 'printing'"

- isLiteral

	  self detect: [:element | element isLiteral not] ifNone: [^true].
	  ^ false.

- printOn: aStream
    aStream nextPut: $(.
	  self do: [:element | element printOn: aStream. aStream space].
	  aStream nextPut: $).


- storeOn: aStream
	  "Use the literal form if possible."

    self isLiteral
		  ifTrue:
			  [aStream nextPut: $#; nextPut: $(.
		  	self do:
			  	[:element |
				  element printOn: aStream.
			  	aStream space].
			  aStream nextPut: $)]
		  ifFalse: [super storeOn: aStream].

"Array methodsFor: 'private'"

- hasLiteralSuchThat: litBlock
    "Answer true if litBlock returns true for any literal in this array, even if imbedded in further array structure.
	   This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
    | lit |
    1 to: self size do:
      [:index | lit := self at: index.
      (litBlock value: lit) ifTrue: [^ true].
      (lit class == Array and: [lit hasLiteralSuchThat: litBlock]) ifTrue: [^ true]].
    ^ false.

- replaceFrom: start to: stop with: replacement startingAt: repStart
    "Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart,
	  in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional.
	  See Object documentation whatIsAPrimitive."
	JVM primitive: 105.
