" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Magnitude < #Number

"intervals"
- to: stop
  ^ Interval from: self to: stop by: 1.

- to: stop do: aBlock
  | nextValue |
  nextValue := self.

  [nextValue <= stop]
    whileTrue:
      [aBlock value: nextValue.
      nextValue := nextValue + 1].