;;; wand-helper.el --- Helpers for Wand

;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)

;; Author: Ha-Duong Nguyen <cmpitgATgmail>
;; Keywords: extensions, tools

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


;;; Code:

(defun wand-helper:string-empty? (str)
  "Determine if a string is empty."
  (= 0 (length str)))

(defalias 'wand-helper:is-selecting? 'region-active-p
  "Determine if there is currently a selection.")

(defun wand-helper:get-selection ()
  "If there is currently a selection, return it.  Otherwise
return an empty string."
  (if (wand-helper:is-selecting?)
    (buffer-substring (region-beginning)
                      (region-end))
    ""))

(defun wand-helper:eval-string (string)
  "Eval a string non-interactively."
  (eval (read string)))

(provide 'wand-helper)
;;; wand-helper.el ends here
