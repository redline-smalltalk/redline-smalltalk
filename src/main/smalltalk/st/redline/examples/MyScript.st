
| var var2 |

var := MyObject new.

^ var class == var2
    ifTrue: [ true ]
    ifFalse: [ self class ].
