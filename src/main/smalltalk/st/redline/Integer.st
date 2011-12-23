" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Number < #Integer

"arthimetic"
- + aNumber
	<primitive: 21>

- - aNumber
	<primitive: 22>

- < aNumber
	<primitive: 23>

- \> aNumber
	<primitive: 24>

- <= aNumber
	<primitive: 25>

- >= aNumber
	<primitive: 26>

- = aNumber
	<primitive: 27>

- * aNumber
  <primitive: 29>

- / aNumber
  <primitive: 30>

"testing"
- isInteger
  ^ true.

"truncation"
- floor
  "do nothing"

- truncated
  "do nothing"