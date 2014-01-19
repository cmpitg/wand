# Wand #

Wand is an extension that allows users to select a piece of text and perform
actions based on predefined patterns.  Wand is inspired by Xiki and Acme
editor.

TODO:

* Screencast
* Make `wand` available in MELPA

## Requirements ##

* Emacs 23+

* Wand depends of `cl` library (bundled with Emacs 23+) and
  [Dash](https://github.com/magnars/dash.el).

## Installation ##

Make sure you have Dash installed.  If not, please follow to Dash's
[installation instruction](https://github.com/magnars/dash.el#installation) to
install Dash.

Currently, you could try out Wand's latest working version with the
instructions below:

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

## Usage ##

### Example ###

It's probably the best to have a look at an example.

`wand:execute` is a command (an `interactive` function) that takes the current
selection (if selection is active) or a string and performs an action based on
current rules residing in `wand:*rules*`.  I use this command so frequently
that I bind it to `<C-return>` and `<C-mouse-1>`.

```lisp
(require 'wand)
(global-set-key (kbd "<C-return>")       'wand:execute)
(global-set-key (kbd "<C-mouse-1>")      'wand:execute)
(global-set-key (kbd "<C-down-mouse-1>")  nil)
```

Then, I want whenever `wand:execute` is called upon a selection of a string
that:

* starts with `$ command`, `command` is executed as a shell command, and
  its output is taken back to Emacs as a popup buffer (using
  [Popwin](https://github.com/m2ym/popwin-el) library),

  ```lisp
  (wand:add-rule-by-pattern :match "\\$ "
                            :capture :after
                            :action popup-shell-command)
  ```

* starts with `http://an-url` or `https://a-url`, `a-url` is opened in
  Firefox with HTTP or HTTPS as its protocol respectively:

  ```lisp
  (wand:add-rule-by-pattern :match "https?://"
                            :capture :whole
                            :action open-url-in-firefox)
  ```

* starts with `file:path-to-a-file`, that corresponding file is open with
  Emacs.  This is particularly useful when using with
  [`openwith`](http://www.emacswiki.org/emacs/OpenWith):

  ```lisp
  (wand:add-rule-by-pattern :match "file:"
                            :capture :after
                            :action find-file)
  ```

* starts with `#> an-emacs-lisp-expression`, brackets are added to that
  expression if necessary and it's then evaluated:

  ```lisp
  (wand:add-rule-by-pattern :match "file:"
                            :capture :after
                            :action add-bracket-and-eval)
  ```

Comments are skipped when the pattern-matching process is performed.  The code
is pretty self-explanatory.

## License ##

This project along with its source code and all materials are released under
the terms of the GNU General Public License 3.0 (GPLv3).  See
[COPYING](COPYING) for more details.

Copyright (C) 2014  Duong Nguyen <cmpitgATgmail>
