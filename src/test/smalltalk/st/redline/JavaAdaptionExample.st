import: 'com.friendly.Test' as: 'Test'.
import: 'java.util.ArrayList' as: 'JList'.

Transcript  show: '1aa'; cr.
Test new.
Test new toString.
Transcript  show: '2bb'; cr.
Transcript show: (Test new) toString; cr.
Transcript  show: '3cc'; cr.

[
    | a |
    a := OrderedCollection new.
    a add: Test new.
    Test new equals: a.
] value.

Transcript  show: '4dd'; cr.

Transcript show: [
  | a |
  a := OrderedCollection new.
  a add: Test new.
  Test new equals: a.
] value.
Transcript cr.

Transcript  show: '5ee'; cr.

Transcript show: #(Test new).  Transcript cr.
Transcript show: (OrderedCollection with: (Test new)).  Transcript cr.
Transcript show: (Test new equals: (OrderedCollection with: (Test new))).

Transcript  show: ([  | a | a := OrderedCollection new.  a add: Test new. Test new equals: a. ] value ifTrue:['T'] ifFalse:['F']). Transcript cr.
Transcript  show: '6ff'; cr.

JList new.
[|a| a := JList new. a add: (OrderedCollection with: (Test new)). a] value size = 1 ifFalse:[Object new error: 'size should have been 1'].

[|a| a := JList with: (OrderedCollection with:(OrderedCollection with: (Test new))). a] value size = 1 ifFalse:[Object new error: 'size should have been 1'].

[|a b c | 
	b := Test new. 
	a := JList new. 
	a add: (OrderedCollection with: b). 
	c := a contains: (OrderedCollection with: b). 
	c ifFalse:[Object new error: 'contains should have been true']. 
] value. 

[|a b c | 
	a := JList new. 
	a add: (OrderedCollection with: Test new). 
	c := a contains: (OrderedCollection with: Test new). 
	c ifTrue:[Object new error: 'contains should have been false']. 
] value. 


[|a b  | 
	a := JList new. 
	a add: (OrderedCollection with: Test new). 
	a add: (OrderedCollection with: Test new). 
	b := JList new. 
	b addAll: (OrderedCollection with: a). 
	(b size = 2) ifFalse:[Object new error: 'size should have been 2']. 
] value. 
