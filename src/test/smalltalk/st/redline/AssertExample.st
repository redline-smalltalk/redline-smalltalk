| i |

"Transcript show: 'Hello World.'; cr; cr."

Object testClassMethod.

i := 1.
Assert assert: [ i = 1 ] withMessage: 'should not fail'.

Assert assert: [ i = 2 ] withMessage: 'should fail'.
