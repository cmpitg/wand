;;; wand.el --- Magic wand for Emacs - Selecting and executing

;; Copyright (C) 2012-2013  Duong H. Nguyen

;; Author: Duong H. Nguyen <cmpitgATgmail>
;; Keywords: extensions, tools
;; URL: https://github.com/cmpitg/wand

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
;; TODO: Screencast

;;; Requirements

;; Common Lisp Extensions, bundled with all recent versions of Emacs.

;;; Installation:

;; The easiest way to get `wand' is to install it via ELPA or `el-get'.  If
;; you prefer `el-get' to ELPA, simply do
;;
;;   (el-get-install 'wand)
;;
;; TODO: Alternatively, make sure you have MELPA repository added to
;; `package-archives' and simply call `package-install':
;;
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;   (package-initialize)
;;   (package-install 'wand)
;;
;; Then `require' it:
;;
;;   (require 'wand)

;;; Usage:

;; To add a rule:
;;
;;   (wand:add-rule :match match-regexp
;;                  :capture capture-rule
;;                  :action action)
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

;;; Code:

(require 'cl)

(defvar wand:*rules*
  '()
  "The list of rules to for pattern-based action.  Rules are
added using `wand:add-rule' and remove using `wand:remove-rule'.
Each element is a pair: `\(check-fn . action-fn\)` where:

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

(provide 'wand)
;;; wand.el ends here
