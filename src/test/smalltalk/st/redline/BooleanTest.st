TestCase < #BooleanTest

-test
  self testTrue.
  self testFalse.

- testTrue
  true ifTrue:  [] 
       ifFalse: [ Assertion assert: [ false ] withMessage: 'true is false' ].

- testFalse
  false ifTrue:  [ Assertion assert: [ false ] withMessage: 'false is true']
        ifFalse: [].
