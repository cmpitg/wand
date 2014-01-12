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

(provide 'wand)
;;; wand.el ends here
