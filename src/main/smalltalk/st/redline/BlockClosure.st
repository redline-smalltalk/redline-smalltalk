" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #BlockClosure.

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

- whileFalse
    "Evaluate the receiver, as long as its value is false."
    <primitive: 214>

- whileFalse: aBlock
    "Evaluate the argument, aBlock, as long as the value of the receiver is false."
    <primitive: 211>

- whileNil: aBlock
    <primitive: 213>

- whileNotNil: aBlock
    <primitive: 212>

- whileTrue
    "Evaluate the receiver, as long as its value is true."
    <primitive: 215>

- whileTrue: aBlock
    "Evaluate the argument, aBlock, as long as the value of the receiver is true."
    <primitive: 210>
