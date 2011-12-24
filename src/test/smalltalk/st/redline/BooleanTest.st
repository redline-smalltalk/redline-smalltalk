TestCase < #BooleanTest

-test
  self testTrue.
  self testFalse.

- testTrue
  true ifTrue:  [] 
       ifFalse: [ self assertTrue: [ false ] withMessage: 'true is false' ].

- testFalse
  false ifTrue:  [ self assertTrue: [ false ] withMessage: 'false is true']
        ifFalse: [].
