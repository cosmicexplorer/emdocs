emdocs
===========

A minor mode for emacs to create buffers readily editable by multiple users over
the internet. (Pronounced "M-dox.")

Currently ~~functional, need to iron out a few bugs before putting it on MELPA.~~ rewriting as a rust grpc server.

# TODO
- [x] Remove cruft like "associating" a buffer with an id.
- [ ] Have to synchronize keypresses, or set interval to broadcast entire buffer; otherwise buffers
  can get out of sync if one fails to note a keypress, for example.

# License
[AGPL 3.0 **or any later version**](./LICENSE)
