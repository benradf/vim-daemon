# vim-daemon

Ostensibly this was a project to control and extend vim using Haskell rather
than the inbuilt Vimscript. In the end I didn't implement much in the way of
actual functionality and it was mostly a playground for experimenting with
streams and [lazy
parsing](https://github.com/benradf/vim-daemon/blob/master/Lex.hs#L202-L206).

It works by launching a Haskell process that communicates with vim over a job
channel. The Haskell process can lazily request chunks of text from vim [using
the getline
function](https://github.com/benradf/vim-daemon/blob/master/Main.hs#L66-L67).
This allows efficient scanning and parsing of text locally around the cursor,
even though the whole file might be very large in size.
