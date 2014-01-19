# Wand #

Wand is an extension that allows users to select a piece of text and perform
actions based on predefined patterns.  Wand is inspired by Xiki and Acme
editor.

TODO:

* Takes from source code
* Screencast
* Make `wand` available in MELPA

## Installation ##

Currently, you could try out Wand by latest working version and `require` in
your Emacs init file (typically residing at `~/.emacs`, `~/.emacs.el`, or
`~/.emacs.d/init.el`):

* Download and extract the
  [latest working version](http://reference-error.org/emacs/wand-latest.zip):

  ```sh
  cd /some/dir/you/place/your/emacs/libs
  wget http://reference-error.org/emacs/wand-latest.zip
  unzip wand-latest.zip
  ```

* Add Wand directory to your `load-path` and `require`:

  ```sh
  # Edit your `~/.emacs`, `~/.emacs.el`, or `~/.emacs.d/init.el`
  (add-to-list 'load-path "/path/to/your/wand/directory")
  (require 'wand)
  ```

## License ##

This project along with its source code and all materials are released under
the terms of the GNU General Public License 3.0 (GPLv3).  See
[COPYING](COPYING) for more details.

Copyright (C) 2014  Duong Nguyen <cmpitgATgmail>
