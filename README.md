# Wand #

Wand is an extension that allows users to select text and perform actions
based on predefined patterns.  Wand is inspired by Xiki and Acme editor.

* Screencast (Upcoming - **Jan 2017**)

## Requirements ##

* Emacs 24+

* Emacs Lisp CL library (bundled with Emacs 23+)

* [Dash](https://github.com/magnars/dash.el) - for list processing

* [s.el](https://github.com/magnars/s.el) - for string processing

## Installation ##

Thanks to [@yasuyk](https://github.com/yasuyk) Wand is available in Melpa.
Installation process is now as simple as `M-x package-install RET wand RET`.

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
  (wand:add-rule-by-pattern :match "#> "
                            :capture :after
                            :action add-bracket-and-eval)
  ```

Comments are skipped when the pattern-matching process is performed.  The code
is pretty self-explanatory.

## Thanks ##

Special thanks to:

* [@yasuyk](https://github.com/yasuyk) for making Wand available in Melpa.

* [@MatthewDarling](https://github.com/MatthewDarling) for correcting code
  example in README.

* [@syl20bnr](https://github.com/syl20bnr) for the issue report.

## License ##

This project along with its source code and all materials are released under
the terms of the GNU General Public License 3.0 (GPLv3).  See
[COPYING](COPYING) for more details.

Copyright (C) 2014-2017  Ha-Duong Nguyen <cmpitgATgmail>
