emdocs
===========

A minor mode for emacs to create buffers readily editable by multiple users over
the internet. (Pronounced "M-dox.")

# NO LONGER ACTIVE

While this was fun to work on, I don't really see the use case for it and it doesn't really excite me, so I'm abandoning this for now. Sorry!

# INTRO

Currently functional, need to iron out a few bugs before putting it on MELPA.

* emdocs-disconnect/emdocs-disconnect-all doesn't disconnect servers sometimes
  after connecting to a client.
* Remote client sometimes freezes upon connection.
* Have to synchronize keypresses, or set interval to broadcast entire buffer;
  otherwise buffers can get out of sync if one fails to note a keypress, for
  example.
* Need to get windows support.
