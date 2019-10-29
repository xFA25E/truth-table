;;; truth-table.el --- Generate truth tables        -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate truth table
;; just M-x truth-table RET
;; and give it a boolean Lisp expression

;;; Code:

(defvar truth-table-allowed-functions
  (list 'and 'or 'not))

(defvar truth-table-buffer-name
  "*Truth Table*")

(defun truth-table-extract-variables (expression)
  "Extract variables from `EXPRESSION'."
  (let (variables)
    (dolist (item (cl-rest expression))
      (if (listp item)
          (dolist (item (truth-table-extract-variables item))
            (cl-pushnew item variables))
        (cl-pushnew item variables)))
    (cl-sort variables #'string-lessp :key #'symbol-name)))

;; (truth-table-extract-variables '(and a (or b c) (not (and w t (or t c b)))))

(defun truth-table-valid-expression-p (expression)
  "Return t if `EXPRESSION' contain a valid expression.
Every head of a node should be on of `truth-table-allowed-functions'"
  (cl-destructuring-bind (first &rest rest) expression
    (and (memq first truth-table-allowed-functions)
         (cl-loop for item in rest
                  if (and (listp item)
                          (not (truth-table-valid-expression-p item)))
                  return nil
                  finally return t))))

;; (truth-table-valid-expression-p '(and a (or b c) (not (and w t (or t c b)))))

(defun truth-table-generate-function (variables expression)
  "Create a function based on `VARIABLES' and a boolean `EXPRESSION'."
  (eval `(lambda ,variables ,expression)))

(defun truth-table-generate-inputs (count)
  "Generate `COUNT' permutations of booleans."
  (if (= 1 count)
      (list (list nil) (list t))
    (mapcan
     (lambda (item)
       (mapcar (lambda (next-item)
                 (cons item next-item))
               (truth-table-generate-inputs (1- count))))
     (list nil t))))

(defun truth-table-generate (expression)
  "Generate truth table alist from `EXPRESSION'."

  (unless (truth-table-valid-expression-p expression)
    (error "%S is not a valid expression!" expression))

  (let* ((variables (truth-table-extract-variables expression))
         (variables-count (length variables))
         (function (truth-table-generate-function variables expression))
         (inputs (truth-table-generate-inputs variables-count)))
    (cons
     (list variables expression)
     (mapcar (lambda (values)
               (list values (apply function values)))
             inputs))))

;; (truth-table-generate '(and a (or b c)))

(defun truth-table-generate-plot-string (truth-table)
  "Return plot string based on `TRUTH-TABLE'."
  (let ((result ""))
    (dolist (line truth-table)
      (dolist (var (cl-first line))
        (setq result (format "%s%4s |" result var)))
      (setq result (format "%s %s\n" result (cl-second line))))
    result))

;; (truth-table-generate-plot-string (truth-table-generate '(and a (or b c))))

(defun truth-table (expression)
  "Generate a truth table from a Lisp boolean `EXPRESSION'."
  (interactive "xeLisp expression: ")
  (let* ((truth-table-alist (truth-table-generate expression))
         (truth-table-string (truth-table-generate-plot-string
                              truth-table-alist)))
    (pop-to-buffer truth-table-buffer-name)
    (erase-buffer)
    (insert truth-table-string)))

(provide 'truth-table)
;;; truth-table.el ends here
