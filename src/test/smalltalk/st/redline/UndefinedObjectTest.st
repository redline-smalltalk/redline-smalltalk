TestCase < #UndefinedObjectTest
  
-test
  self testIsNil.
  self testNotNil.

- testIsNil
  self assertTrue: nil isNil withMessage: 'isNil failure'.

- testNotNil
  self assertFalse: nil notNil withMessage: 'notNil failure'.
  