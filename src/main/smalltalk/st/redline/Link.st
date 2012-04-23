" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Object < #Link
  instanceVariableNames: 'nextLink'.

"converting"

- asLink
    ^ self.

"accessing"

- nextLink
    ^ nextLink.

- nextLink: aLink
    ^ nextLink := aLink.

"instance creation"

+ nextLink: aLink
    ^ self new nextLink: aLink; yourself.
