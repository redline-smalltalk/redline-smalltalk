" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

SequenceableCollection < #Interval
	instanceVariableNames: 'start stop step';
	classVariableNames: '';
	classInstanceVariableNames: '';
	poolDictionaries: '';
	category: ''.

+ from: startInteger to: stopInteger
    ^ self new
        setFrom: startInteger
        to: stopInteger
        by: 1.

+ from: startInteger to: stopInteger by: stepInteger
    ^ self new
        setFrom: startInteger
        to: stopInteger
        by: stepInteger.

+ new
    <primitive: 70>
    self isVariable ifTrue: [ ^ self new: 0 ].

- setFrom: startInteger to: stopInteger by: stepInteger
    start := startInteger.
    stop := stopInteger.
    step := stepInteger.

- do: aBlock
    | aValue index size |
    index := 0.
    size := self size.
    [index < size]
        whileTrue: [aValue := start + (index * step).
            index := index + 1.].
            aBlock value: aValue].