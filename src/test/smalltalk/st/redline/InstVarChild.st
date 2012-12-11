
self import: 'st.redline.*'.

InstVarParent < #InstVarChild.

- initialize
    super initialize.

- showName
  Transcript show: self name; cr.
