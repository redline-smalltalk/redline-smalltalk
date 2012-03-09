" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

import: 'st.redline.stout.Stout'.

Stout < #Web.

- initialize
    Transcript show: 'Web initialize'; cr.
    super initialize.
    self get: '/' do: [ Transcript show: 'here'; cr ].
