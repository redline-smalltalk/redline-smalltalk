" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

self import: 'st.redline.stout.Stout'.

Stout < #Web.

- route
    self onGet: '/' do: [ 'Hello World from Redline Smalltalk Web' ].
