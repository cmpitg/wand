;;; wand.el --- Magic wand for Emacs - Selecting and executing

;; Copyright (C) 2014  Duong H. Nguyen

;; Author: Duong H. Nguyen <cmpitgATgmail>
;; Keywords: extensions, tools
;; URL: https://github.com/cmpitg/wand
;; Package-Requires: ((dash "2.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Wand is an extension that allows users to select a piece of text and
;; perform actions based on predefined patterns.  Wand is inspired by Xiki[1]
;; and Acme editor[2].
;;

;;; Dependencies:

;; Note that you don't need to worry about these dependencies if you're using
;; an Emacs package manager such as ELPA or el-get.
;;
;; * Common Lisp Extensions, bundled with all recent versions of Emacs.
;;
;; * A modern list library for Emacs Lisp: magnars's excellent Dash[4] -- to
;;   promote better ways to write Emacs Lisp.

;;; Installation:

;; Thanks to @yasuyk, Wand is available in MELPA.  Make sure you have MELPA
;; repository added to `package-archives' and simply call `package-install':
;;
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;   (package-initialize)
;;   (package-install 'wand)
;;
;; Or `M-x package-install RET wand RET`.
;;
;; Then `require' it:
;;
;;   (require 'wand)

;;; Usage:

;; To add a rule:
;;
;;   (wand:add-rule-by-pattern :match match-regexp
;;                             :capture capture-rule
;;                             :action action)
;;
;; Or set the value of `wand:*rules*' directly with `wand:create-rule'.  A
;; piece of code example's worth thousands dry words:
;;
;;   (setq wand:*rules*
;;         (list (wand:create-rule :match "\\$ "
;;                                 :capture :after
;;                                 :action popup-shell-command)
;;               (wand:create-rule :match "https?://"
;;                                 :capture :whole
;;                                 :action open-url-in-firefox)
;;               (wand:create-rule :match "file:"
;;                                 :capture :after
;;                                 :action find-file)
;;               (wand:create-rule :match "#> "
;;                                 :capture :after
;;                                 :action (lambda (string)
;;                                           (eval (read string))))))
;;
;; Means: if you call `wand:execute'
;;
;; * With `;; $ ls` in Lisp mode, a window[3] is popup with output of `ls` as
;;   its content,
;;
;; * With `### $ ls` in Python mode, the result is similar as in Lisp mode,
;;
;; * With `http://google.com` or `https://google.com`, your Firefox will open
;;   Google home page (with HTTP and HTTPS respectively),
;;
;; * With `file:~/tmp/tmp.el`, your `$HOME/tmp/tmp.el` is open in Emacs,
;;
;; * With `#> (message-box "¡Hola a todos!")`, a message box is displayed with
;;   the text "¡Hola a todos!".
;;
;; * With any other string, the string is `eval'-ed with `wand:eval-string'.
;;   This the default action for all unmatched strings.
;;
;; Of course `popup-shell-command' and `open-url-in-firefox' have to be
;; defined.
;;
;; It's recommended to bind `wand:execute' to a key stroke and/or mouse for
;; better usage pattern.  Here is a full example, take from my configuration:
;;
;;   (require 'wand)
;;   (global-set-key (kbd "<C-return>")       'wand:execute)
;;   (global-set-key (kbd "<C-S-return>")     'wand:execute-current-line)
;;   (global-set-key (kbd "<C-mouse-1>")      'wand:execute)
;;   (global-set-key (kbd "<C-down-mouse-1>")  nil)
;;
;;   (dolist (rule (list (wand:create-rule :match "\\$ "
;;                                         :capture :after
;;                                         :action $popup-shell-command)
;;                       (wand:create-rule :match "https?://"
;;                                         :capture :whole
;;                                         :action $open-url-in-firefox)
;;                       (wand:create-rule :match "file:"
;;                                         :capture :after
;;                                         :action toolbox:open-file)
;;                       (wand:create-rule :match "#> "
;;                                         :capture :after
;;                                         :action $add-bracket-and-eval)
;;                       (wand:create-rule :match "window:"
;;                                         :capture :after
;;                                         :action $switch-to-window)
;;                       (wand:create-rule :match "eshell-cd:"
;;                                         :capture :after
;;                                         :action $change-dir-in-eshell)
;;                       ))
;;     (wand:add-rule rule))
;;
;; Note that `wand:execute' is an interactive command.  Thus you can call it
;; just like other commands.
;;
;; Rule match order: last rule is checked first.  It means if you have 2 rules
;; that share the same match regexp, the latter is chosen.
;;

;;; How it works:

;; `wand:*rules*' is an alist of rules with format `(check-fn . action-fn)`.
;; The input string passed to `wand:execute' is passed to all the `check-fn`s
;; in the order they are added.  If `check-fn` returns `t', `action-fn` is
;; called with the same input string as its only argument.  For detailed
;; documentation, see docstrings of `wand:*rules*' and `wand:execute'.

;;; Notes and references:

;; [1] http://xiki.org/
;; [2] http://acme.cat-v.org/
;; [3] This is a window in Emacs terms, not a window in your GUI system.
;; [4] https://github.com/magnars/dash.el

;;; Code:

(require 'cl)
(require 'dash)

(require 'wand-helper)

(defvar wand:*rules*
  '()
  "The list of rules to for pattern-based action.  Rules are
added using `wand:add-rule'/`wand:add-rule-by-pattern' and remove
using `wand:remove-rule'/`wand:remove-rule-by-pattern'.  Each
element is a pair: `\(check-fn . action-fn\)` where:

* `check-fn` is a one-argument function, taking a string and
  determining if the string satisfies the rule.

* `action-fn` is a one-argument function, taking a string and
  execute the action based on that string if its corresponding
  `check-fn` is satisfied.")

(defun wand:get-rule-action (string)
  "Determine if a string matches a predefined rule.  If it does,
return the function corresponding to that rule's action;
otherwise return `nil'."
  (->> wand:*rules*
    (-first (lambda (rule-pair)
              (let ((rule-check-fn (car rule-pair)))
                (funcall rule-check-fn string))))
    cdr))

(defun wand:get-from-match-to-end (string regexp)
  "Extract the substring from the end of the current match \(by
`regexp`\) til the end of the string.

E.g.

\(wand:get-from-match-to-end \";; $ ls -lahF\" \"^[; ]*\\\\$ \"\)
;; => \"ln -lahF\"
"
  (string-match regexp string)
  (substring string (or (match-end 0) 0)))

(defun wand:eval-string (&optional string)
  "Add outer-most surrounding bracket if necessary and eval the
string.  This function may be called interactively.  If it's in
interactive mode and there's current a selection, the selection
is evaluated.

This function is convenient when being called interactively or
quickly eval a selection which contains Emacs Lisp code.

E.g.

\(wand:eval-string \"message \\\"¡Hola mundo!\\\"\"\)
;; => ¡Hola mundo!

\(wand:eval-string \"\(message \\\"¡Hola mundo!\\\"\)\"\)
;; => ¡Hola mundo!
"
  (interactive)
  (let* ((preprocessed-sexp (cond ((not (wand-helper:string-empty? string))
                                   string)
                                  ((wand-helper:is-selecting?)
                                   (wand-helper:get-selection))
                                  (t
                                   (read-string "Command: "))))
         (sexp (if (not (and (s-starts-with? "(" preprocessed-sexp)
                             (s-ends-with?   ")" preprocessed-sexp)))
                 (format "(%s)" preprocessed-sexp)
                 preprocessed-sexp)))
    (wand-helper:eval-string sexp)))

(defmacro* wand:create-rule (&key (skip-comment t)
                                  match
                                  capture
                                  (action wand:eval-string))
  "This macro provides a simplified, declarative, yet very
effective way to create a pattern-action rule without having to
construct the `\(check-fn . action-fn\)` cons manually.

`wand:create-rule' takes several arguments describing the process
of matching and extracting a string and the action performed upon
it:

* `match` is a regexp to test whether the input string (passed to
  `check-fn`) satisfies current rule.  The input string is
  checked with `string-match-p'.

* `capture` determines how the input string is extracted to be
  passed to `action-fn`.  It takes one of the following types of
  value:

  - `:whole' - means the original string is passed to `action`.

  - `:after' - means only substring after the match part is
    passed to `action`.

  - `nil' - means an empty string is passed to `action`.

  - a regular expression _with capture group_.  `string-match'
    followed by `match-string' are called to extract the first
    captured group which is then is passed to `action`.

  - a function - that takes the original string and returns what
    `action` wants to process

* `skip-comment` takes either `t' or `nil', determining if line
  comment syntax of current mode is skipped when matching
  happens.  By default, `skip-comment` is `t', which means
  comments are skipped.

* `action` is `action-fn`, a function that will be called when
  the input string is matched.  `action` is `wand:eval-string' by
  default.

E.g.

Call `\(message-box something\)` when input string is `#> something`:

  \(wand:create-rule :match \"#> \"
            :capture :after
            :action message-box\)

Browse a HTTP/HTTPS web page when input string is
`http://some-url` or `https://some-url`:

  \(wand:create-rule :match \"https?://\"
            :capture :whole
            :action browse-url\)

Open file when input string is `file:///path/to/your-file`:

  \(wand:create-rule :match \"file:///\"
            :capture :after
            :action find-file\)
"
  (let* ((match-regexp `(format "^[%s ]*%s"
                                comment-start
                                ,match))

         (extract-regexp (cond
                          ;; Capture the string after position the regexp
                          ;; matches
                          ((equalp :after capture)
                           `(format "^[%s ]*%s\\(.*\\)$"
                                    comment-start
                                    ,match))

                          ;; Capture the whole string right after comment
                          ((equalp :whole capture)
                           `(format "^[%s ]*\\(%s.*\\)$"
                                    comment-start
                                    ,match))

                          ;; No capturing
                          ((or (null capture)
                               (not (equalp 'string (type-of capture))))
                           "")

                          ;; Regexp is specified manually
                          (t
                           capture)))

         (rule-check-fn `(lambda (string)
                           ,match       ; Using `match` as its docstring
                           (string-match-p ,match-regexp string)))

         (action-fn `(lambda (string)
                       (let* ((extract (progn
                                         (string-match ,extract-regexp string)
                                         (match-string 1 string))))
                         (,action extract)))))
    `(cons ,rule-check-fn ,action-fn)))

(defun wand:add-rule (rule)
  "Add a rule created by `wand:create-rule' to `wand:*rules*'.

Usage: `\(wand:add-rule \(wand:create-rule ...\)\)`.

Note: If 2 rules share the same pattern, the one which is added
latter takes higher precedence."
  (wand:remove-rule (car rule))
  (setq wand:*rules* (cons rule wand:*rules*)))

(defun wand:remove-rule (check-fn)
  "Remove a rule from `wand:*rules*' based on its `check-fn` and
return `wand:*rules*'."
  (setq wand:*rules* (-filter (lambda (rule)
                                (let ((fn (car rule)))
                                  (not (equal fn check-fn))))
                              wand:*rules*)))

(defun wand:remove-rule-by-pattern (pattern)
  "Remove a rule from `wand:*rules*' based on its matching
pattern and return `wand:*rules*'."
  (setq wand:*rules* (-filter (lambda (rule)
                                (let ((fn (car rule)))
                                  (not (equal pattern (documentation fn)))))
                              wand:*rules*)))

(defun* wand:execute (&optional (string-to-execute ""))
  "Execute a string based on predefined rules stored in
`wand:*rules*.  If no rules are found, `eval' the string using
`wand:eval-string' function.

This function could be called interactively.  The string to
execute is determined as follow:

* If this function is called non-interactively, it's the argument
  that is passed to this function,

* If there is currently a selection, it's the current selected
  text,

* Otherwise, prompt and get the result as its value.

The rules are defined in `wand:*rules*' variable.  Use
`wand:add-rule' or `wand:add-rule-by-pattern' to add rule,
`wand:remove-rule' or `wand:remove-rule-by-pattern' to remove
rule.

For strings that are not matched by any rules, they're called
with `wand:eval-string' by default.

E.g.

\(some-func \"message \\\"Hello World\\\"\"\)
\(some-func \"\(message \\\"Hello World\\\"\\)\"\)
;; Both echo \"Hello World\" in echo area
"
  (interactive)
  (let* ((string (cond ((not (wand-helper:string-empty? string-to-execute))
                        string-to-execute)
                       ((wand-helper:is-selecting?)
                        (wand-helper:get-selection))
                       (t
                        (read-string "String: "))))
         (action (or (wand:get-rule-action string)
                     #'wand:eval-string)))
    (funcall action string)))

(defun wand:execute-current-line ()
  "Call `wand:execute' on current line."
  (interactive)
  (wand:execute (thing-at-point 'line)))

(defmacro* wand:add-rule-by-pattern (&key (skip-comment t)
                                          match
                                          capture
                                          (action wand:eval-string))
  "This function is a shorthand for

\(add-to-list 'wand:*rules*
             \(wand:create-rule :skip-comment skip-comment
                               :match match
                               :capture capture
                               :action action\)\)

Besides, if there are 2 rules that share the same `match', the
one which's added later takes effect."
  `(setq wand:*rules* (wand:remove-rule-by-pattern ,match))
  `(wand:add-rule (wand:create-rule :skip-comment ,skip-comment
                                    :match ,match
                                    :capture ,capture
                                    :action ,action)))


(provide 'wand)
;;; wand.el ends here
