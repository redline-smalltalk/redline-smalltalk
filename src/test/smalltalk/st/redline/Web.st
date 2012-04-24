" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

import: 'st.redline.stout.Stout'.

Stout < #Web.

- initialize
    super initialize.
    self whenGet: '/' do: [ 'Hello World from Smalltalk Web' ].
