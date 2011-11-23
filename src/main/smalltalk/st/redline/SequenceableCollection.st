" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Collection < #SequenceableCollection

"enumerating"
- do: aBlock
    1 to: self size do:
        [:index | aBlock value: (self at: index)].