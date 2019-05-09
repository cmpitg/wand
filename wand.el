;;; wand.el --- Magic wand for Emacs - Select and execute  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019 Ha-Duong Nguyen (@cmpitg)

;; Author: Ha-Duong Nguyen <cmpitgATgmail>
;; Keywords: extensions, tools
;; URL: https://github.com/cmpitg/wand
;; Package-Requires: ((dash "2.15.0") (s "0.1.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Wand is an extension that allows users perform actions on a region based on
;; predefined patterns.  Wand is inspired by Xiki[1] and Acme[2].
;;

;;; Dependencies:

;; Note that you don't need to worry about these dependencies if you're using
;; an Emacs package manager.
;;
;; * Common Lisp Extensions, bundled with all recent versions of Emacs.
;;
;; * A modern list library for Emacs Lisp: @magnars's excellent Dash[4] -- to
;;   promote better ways to write Emacs Lisp.
;;
;; * The long lost Emacs string manipulation library: s.el[5] -- also by
;;   @magnars.

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

;; Wand works by going through a list of rules, stored in the `wand:*rules*'
;; variable (which should be customized to be useful).  Each rule has the
;; meaning of "if satisfied, perform an action; otherwise pass to the next
;; rule" and is a cons of `(rule-check-fn . action-fn)':
;;
;; * `rule-check-fn' is a one-argument function, taking a string and returns
;;   `t' or `nil', determining if the string satisfies the current rule
;;
;; * `action-fn' is also a one-argument function, taking the same string and
;;   performing action(s).
;;
;; After setting the rules, we simply call `wand:execute' with the
;; corresponding string.
;;
;; Manually creating all the rules from ground up is possible but usually
;; tedious.  Hence, Wand provides the `wand:create-rule' helper to facilitate
;; the creation of a rule.  `wand:create-rule' takes several arguments
;; describing the process of matching and extracting a string and the action
;; performed upon.  Essentially, `wand:create-rule' cares about the following
;; questions:
;;
;; * What is the regexp that the string is checked against to determine if the
;;   rule is satisfied?  (The `match' argument)
;;
;; * If the string satisfies the rule, how is it then passed to the action
;;   function?  (The `capture' argument, options are: `:after` - extracting
;;   the string after the match, `:whole` - pass the whole string, `nil` -
;;   pass `nil', or an extractor function taking the string and returning the
;;   processed string)
;;
;; * What is the action that the extracted/processed string is performed upon?
;;   (The `action' argument)
;;
;; * Is the current comment stripped before processing the string?  (The
;;   `skip-comment' argument).  Comments are stripped by default.
;;
;; Here is an example of how it would look like in practice:
;;
;;   (setq wand:*rules*
;;         (list (wand:create-rule :match "\\$ "
;;                                 :capture :after
;;                                 :action #'popup-shell-command)
;;               (wand:create-rule :match "https?://"
;;                                 :capture :whole
;;                                 :action #'open-url-in-firefox)
;;               (wand:create-rule :match "file:"
;;                                 :capture :after
;;                                 :action #'find-file)
;;               (wand:create-rule :match "#> "
;;                                 :capture :after
;;                                 :action #'(lambda (string)
;;                                             (eval (read string))))))
;;
;; When calling `wand:execute <a-string>`, the following would happen:
;;
;; * If the string is `;; $ ls`, call `(popup-shell-command "ls")`
;;
;; * If the string is `http://google.com` or `https://google.com`, call
;;   `(open-url-in-firefox <the-url>)`.
;;
;; * If the string is `file:~/tmp/tmp.el`, call `(find-file "~/tmp/tmp/.el")`.
;;
;; * If the string is `#> (message-box "¡Hola a todos!")`, eval `(message-box
;;   "¡Hola a todos!")`.
;;
;; * With any other string, the string is `eval'-ed with `wand:eval-string'.
;;   This the default action for all unmatched strings.
;;
;; It's recommended to bind `wand:execute' to a key stroke and/or mouse for
;; better usage.
;;

;;; Notes and references:

;; [1] http://xiki.org/
;; [2] http://acme.cat-v.org/
;; [3] A window as in Emacs term
;; [4] https://github.com/magnars/dash.el
;; [5] https://github.com/magnars/s.el

;;; Code:

(require 'cl)
(require 'dash)
(require 's)
(require 'subr-x)

(require 'wand-helper)

(defvar wand:*rules*
  '()
  "The list of rules to for pattern-based action.
Each rule is a cons of the format `\(check-fn . action-fn\)`:

* `check-fn` is a one-argument function, taking a string and
  determining if the string satisfies the rule.

* `action-fn` is a one-argument function, taking a string and
  execute the action based on that string if its corresponding
  `check-fn` returns `t'.")

(defun wand:get-rule-action (string)
  "Determines if a string matches a predefined rule.  If it does,
returns the function corresponding to that rule's action;
otherwise returns `nil'."
  (thread-last wand:*rules*
    (-first (lambda (rule-pair)
              (let ((rule-check-fn (car rule-pair)))
                (funcall rule-check-fn string))))
    cdr))

(defun wand:eval-string (&optional string)
  "Adds a pair of surrounding brackets if necessary and evals the
string.  If no string is passed, take the current region as the
string.

This function is convenient when being called interactively or
quickly eval a region.

E.g.

\(wand:eval-string \"message \\\"¡Hola mundo!\\\"\"\)
;; => ¡Hola mundo!

\(wand:eval-string \"\(message \\\"¡Hola mundo!\\\"\)\"\)
;; => ¡Hola mundo!
"
  (interactive)
  (let* ((preprocessed-sexp (if (or (null string)
                                    (string-empty-p string))
                                (wand-helper:get-selection)
                              string))
         (sexp (if (intern-soft preprocessed-sexp)
                   preprocessed-sexp
                 (format "(%s)" preprocessed-sexp))))
    (unless (string-empty-p (s-trim string))
      (wand-helper:eval-string sexp))))

(defun* wand:create-rule (&key (skip-comment t)
                               match
                               capture
                               (action wand:eval-string))
  "This function provides a simplified and declarative way to
create a pattern-action rule without having to construct the
`\(check-fn . action-fn\)` conses manually.

`wand:create-rule' takes several arguments describing the process
of matching and extracting a string and the action performed upon
it:

* `match` is a regexp to test whether the input string (passed to
  `check-fn`) satisfies the current rule.  The input string is
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

* `skip-comment` takes either `t' or `nil', determining the
  string is stripped of comments.  The comment syntax is defined
  by Emacs's standard `comment-start' and `comment-end'
  variables.

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
  (cl-labels ((rule-check-fn
               (str)
               (let ((comment-start (if (null comment-start) ";" comment-start))
                     (comment-end (if (null comment-end) "" comment-end)))
                 (thread-last (wand-helper:maybe-uncomment-string str skip-comment
                                                                  comment-start comment-end)
                   (string-match-p match))))
              (action-fn
               (str)
               (let* ((comment-start (if (null comment-start) ";" comment-start))
                      (comment-end (if (null comment-end) "" comment-end))
                      (str (wand-helper:maybe-uncomment-string str skip-comment
                                                               comment-start comment-end))
                      (processed-str
                       (cond
                        ((eq :after capture)
                         (let ((prefix (progn (string-match match str)
                                              (match-string 0 str))))
                           (s-chop-prefix prefix str)))

                        ((eq :whole capture)
                         str)

                        ((null capture)
                         nil)

                        ((functionp capture)
                         (funcall capture str))

                        (t
                         (error "`capture` must be :after, :whole, nil, or a function")))))
                 (funcall action processed-str))))
    (cons (function rule-check-fn)
          (function action-fn))))

(defun* wand:execute (&optional (string-to-execute ""))
  "Executes a string based on predefined rules stored in
`wand:*rules*.  If no rules are found, eval the string using
`wand:eval-string' function.

This function could be called interactively.  The string to
execute is determined as follow:

* If this function is called non-interactively, it's the argument
  that is passed to this function,

* If there is currently a selection, it's the current selected
  text,

* Otherwise, do nothing.

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
  (let* ((string (if (or (null string-to-execute)
                         (string-empty-p string-to-execute))
                     (wand-helper:get-selection)
                   string-to-execute))
         (action (or (wand:get-rule-action string)
                     #'wand:eval-string)))
    (unless (string-empty-p (s-trim string))
      (funcall action string))))

(provide 'wand)
;;; wand.el ends here
