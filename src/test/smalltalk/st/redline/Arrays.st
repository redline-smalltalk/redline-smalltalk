
| temp1 temp2 iterableArray |
temp1 := #(1 ('one' 1) 'hello world' symbol $f 3).
temp2 := temp1 at: 3.
Transcript show: temp2; cr; cr.

iterableArray := #(1 2 3 4 5).
iterableArray do: [:each | Transcript show: each; cr. ].
