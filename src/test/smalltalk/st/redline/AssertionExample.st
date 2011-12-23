| i |

"Transcript show: 'Hello World.'; cr; cr."

Object testClassMethod.

i := 1.
Assertion assert: [ i = 1 ] withMessage: 'should not fail'.

Assertion assert: [ i = 2 ] withMessage: 'should fail'.
