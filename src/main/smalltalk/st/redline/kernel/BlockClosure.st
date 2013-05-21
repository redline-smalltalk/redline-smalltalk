"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Object subclass: #BlockClosure.

"category: evaluating"

- value
    "Activate the receiver, a closure activation."
    <primitive: 81>

- value: firstArg
    "Activate the receiver, a closure activation."
    <primitive: 81>

- value: firstArg value: secondArg
    "Activate the receiver, a closure activation."
    <primitive: 81>

- value: firstArg value: secondArg value: thirdArg
    "Activate the receiver, a closure activation."
    <primitive: 81>

- value: firstArg value: secondArg value: thirdArg value: forthArg
    "Activate the receiver, a closure activation."
    <primitive: 81>

"category: controlling"

- whileFalse
    "Evaluate the receiver, as long as its value is false."
    <primitive: 133>

- whileFalse: aBlock
    "Evaluate the argument, aBlock, as long as the value of the receiver is false."
    <primitive: 134>

- whileNil: aBlock
    <primitive: 135>

- whileNotNil: aBlock
    <primitive: 136>

- whileTrue
    "Evaluate the receiver, as long as its value is true."
    <primitive: 137>

- whileTrue: aBlock
    "Evaluate the argument, aBlock, as long as the value of the receiver is true."
    <primitive: 138>
