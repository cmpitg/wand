# Wand #

Wand is an extension that allows users perform actions on a region based on predefined patterns.  Wand is inspired by Xiki[1] and Acme[2].

* Screencast (Upcoming - April/May 2019)

## Requirements ##

* Emacs 25+

* [Dash](https://github.com/magnars/dash.el) - for list processing

* [s.el](https://github.com/magnars/s.el) - for string processing

## Installation ##

Thanks to [@yasuyk](https://github.com/yasuyk) Wand is available in MELPA. Installation process is now as simple as `M-x package-install RET wand RET`.

If you don't have MELPA setup, the following migh suffice:

```elisp
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-install 'wand)
(require 'wand)
```

If you use Emacs 26+, it's recommend to use Wand with `use-package`:

```elisp
(use-package wand
 :ensure t
 :config (...))
```

## Usage ##

Wand works by going through a list of rules, stored in the `wand:*rules*` variable (which should be customized to be useful).  Each rule has the meaning of "if satisfied, perform an action; otherwise pass to the next rule" and is a cons of `(rule-check-fn . action-fn)`:

* `rule-check-fn` is a one-argument function, taking a string and returns `t` or `nil`, determining if the string satisfies the current rule

* `action-fn` is also a one-argument function, taking the same string and performing action(s).

After setting the rules, we simply call `wand:execute` with the corresponding string.

Manually creating all the rules from ground up is possible but usually tedious.  Hence, Wand provides the `wand:create-rule` helper to facilitate the creation of a rule.  `wand:create-rule` takes several arguments describing the process of matching and extracting a string and the action performed upon.  Essentially, `wand:create-rule` cares about the following questions:

* What is the regexp that the string is checked against to determine if the rule is satisfied?  (The `match` argument)

* If the string satisfies the rule, how is it then passed to the action function?  (The `capture` argument, options are: `:after` - extracting the string after the match, `:whole` - pass the whole string, `nil` - pass `nil`, or an extractor function taking the string and returning the processed string)

* What is the action that the extracted/processed string is performed upon? (The `action` argument)

* Is the current comment stripped before processing the string?  (The `skip-comment` argument).  Comments are stripped by default.

Here is an example of how it would look like in practice:

``` elisp
(setq wand:*rules*
      (list (wand:create-rule :match "\\$ "
                              :capture :after
                              :action #'popup-shell-command)
            (wand:create-rule :match "https?://"
                              :capture :whole
                              :action #'open-url-in-firefox)
            (wand:create-rule :match "file:"
                              :capture :after
                              :action #'find-file)
            (wand:create-rule :match "#> "
                              :capture :after
                              :action #'(lambda (string)
                                          (eval (read string))))))
```

When calling `wand:execute <a-string>`, the following would happen:

* If the string is `;; $ ls`, call `(popup-shell-command "ls")`

* If the string is `http://google.com` or `https://google.com`, call `(open-url-in-firefox <the-url>)`.

* If the string is `file:~/tmp/tmp.el`, call `(find-file "~/tmp/tmp/.el")`.

* If the string is `#> (message-box "¡Hola a todos!")`, eval `(message-box "¡Hola a todos!")`.

* With any other string, the string is `eval`-ed with `wand:eval-string`.  This the default action for all unmatched strings.

The string could span through multiple lines.  If `skip-comment` is `t`, comments are stripped from all the lines.

It's recommended to bind `wand:execute` to a key stroke and/or mouse for quick command execution.

```elisp
(global-set-key (kbd "<C-return>")       'wand:execute)
(global-set-key (kbd "<C-mouse-1>")      'wand:execute)
(global-set-key (kbd "<C-down-mouse-1>")  nil)
```

## Thanks ##

Special thanks to:

* [@yasuyk](https://github.com/yasuyk) for making Wand available in Melpa.

* [@MatthewDarling](https://github.com/MatthewDarling) for correcting code
  example in README.

* [@syl20bnr](https://github.com/syl20bnr) for the issue report.

* [@rubikitch](https://github.com/rubikitch)
  for
  [fixing the `string-empty?` typo](https://github.com/cmpitg/wand/pull/9).

## License ##

This project along with its source code and all materials are released under the terms of the GNU General Public License 3.0 (GPLv3).  See [COPYING](COPYING) for more details.

Copyright (C) 2014-2019  Ha-Duong Nguyen <cmpitgATgmail>
