| collection |

collection := LinkedList new.

collection add: 1.
collection add: 2.
collection add: 3.

Assert assert: [ collection size == 3 ] withMessage: 'collection size should be three'.
