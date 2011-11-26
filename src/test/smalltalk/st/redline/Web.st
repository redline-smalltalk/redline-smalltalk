
import: 'st.redline.stout.Stout'.

Stout < #Web

- initialize
    super initialize.
    self get: '/' do: [Transcript show: 'here'; cr].

