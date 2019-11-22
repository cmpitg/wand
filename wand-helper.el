;;; wand-helper.el --- Helpers for Wand

;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)

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

(defun wand-helper:get-selection ()
  "Returns the current region/selection if exists.  If not,
returns an empty string."
  (if (region-active-p)
      (buffer-substring (region-beginning)
                        (region-end))
    ""))

(defun wand-helper:eval-string (string)
  "Evals a string non-interactively."
  (eval (read string)))

(defun* wand-helper:maybe-uncomment-string (str skip-comment?
                                                &key
                                                major-mode-fn)
  "Uncomments a string if `skip-comment?' is `t'.  The comment
syntax is defined by the major mode, denoted by `major-mode-fn'."
  (if skip-comment?
      (let ((comment-start (if (null comment-start) ";" comment-start))
            (comment-end (if (null comment-end) "" comment-end)))
       (with-temp-buffer
         (funcall major-mode-fn)
         (insert str)

         ;; NOTE: This check exists as a hack, due to org-mode's breaking `UNCOMMENT-REGION'
         (unless (= major-mode 'org-mode)
           (uncomment-region (point-min) (point-max)))

         (buffer-string)))
    str))

(provide 'wand-helper)
;;; wand-helper.el ends here
