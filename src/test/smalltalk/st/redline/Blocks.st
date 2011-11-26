| tmp |
tmp := 'Hello World'.
[Transcript show: tmp; cr; cr.] value.

Transcript cr; show: 'whileTrue block, looping 10 times until tmp <= 10'; cr.
tmp := 0.
[
    tmp := tmp + 1.
    Transcript show: tmp; cr.
    tmp <= 10
] whileTrue.

Transcript cr; show: 'whileFalse block, looping 10 times until tmp >= 10'; cr.
tmp := 0.
[
    tmp := tmp + 1.
    Transcript show: tmp; cr.
    tmp >= 10
] whileFalse.

tmp := false.
Transcript cr; show: 'whileTrue block, should output exactly once'; cr.
[
    Transcript show: 'whileTrue only once'; cr.
    tmp
] whileTrue.

tmp := true.
Transcript cr; show: 'whileFalse block, should output exactly once'; cr.
[
    Transcript show: 'whileFalse only once'; cr.
    tmp
] whileFalse.
