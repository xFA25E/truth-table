;;; truth-table.el --- Generate truth tables        -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <xFA25E>
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

(defun truth-table-extract-variables (subexpressions)
  "Extract variables from `SUBEXPRESSIONS'."
  (cl-remove-if #'listp subexpressions))

;; (truth-table-extract-variables '((and a (or b c) (not (and w s (or s c b)))) a (or b c) (not (and w s (or s c b))) (and w s (or s c b)) w (or s c b) s c b))
;; => (a b c s w)

(defun truth-table-extract-subexpressions (expression)
  "Extract subexpressions from `EXPRESSION'."
  (if (listp expression)
      (cl-loop for item in (cl-rest expression)
               nconc (truth-table-extract-subexpressions item) into result
               finally return (cl-delete-duplicates (cons expression result)
                                                    :test #'equal))
    (list expression)))

;; (truth-table-extract-subexpressions '(and a (or b c) (not (and w s (or s c b)))))
;; => ((and a (or b c) (not (and w s (or s c b)))) a (or b c) (not (and w s (or s c b))) (and w s (or s c b)) w (or s c b) s c b)

(defun truth-table-valid-expression-p (expression)
  "Return t if `EXPRESSION' contain a valid expression.
Every head of a node should be in of `truth-table-allowed-functions'"
  (cl-destructuring-bind (first &rest rest) expression
    (and (memq first truth-table-allowed-functions)
         (cl-loop for item in rest
                  if (and (listp item)
                          (not (truth-table-valid-expression-p item)))
                  return nil
                  finally return t))))

;; (truth-table-valid-expression-p '(and a (or b c) (not (and w t (or t c b)))))
;; => t

(defun truth-table-generate-function (variables subexpressions)
  "Create a function based on `VARIABLES' and a boolean `SUBEXPRESSIONS'."
  (eval `(lambda ,variables
           (flet ((and (&rest args) (if (cl-find 0 args) 0 1))
                  (or (&rest args) (if (cl-find 1 args) 1 0))
                  (not (arg) (if (zerop arg) 1 0)))
             (list ,@subexpressions)))))

(defun truth-table-generate-inputs (count)
  "Generate `COUNT' permutations of booleans."
  (cl-loop repeat count
           for result = (list (list 0) (list 1))
           then (cl-loop for seq in result
                         nconc (list (cons 0 seq) (cons 1 seq)))
           finally return result))

(defun truth-table-generate (expression)
  "Generate truth table alist from `EXPRESSION'."

  (unless (truth-table-valid-expression-p expression)
    (error "%S is not a valid expression!" expression))

  (let* ((subexps (nreverse (truth-table-extract-subexpressions expression)))
         (vars (truth-table-extract-variables subexps))
         (vars-count (length vars))
         (func (truth-table-generate-function vars subexps))
         (inputs (truth-table-generate-inputs vars-count)))
    (cons subexps (mapcar (lambda (values) (apply func values)) inputs))))

;; (truth-table-generate '(and a (or b c)))
;; => ((c b (or b c) a (and a (or b c))) (0 0 0 0 0) (1 0 1 0 0) (0 1 1 0 0) (1 1 1 0 0) (0 0 0 1 0) (1 0 1 1 1) (0 1 1 1 1) (1 1 1 1 1))

(defun truth-table (expression)
  "Generate a truth table from a Lisp boolean `EXPRESSION'."
  (interactive "xLisp expression: ")
  (let* ((truth-table-list (truth-table-generate expression)))
    (pop-to-buffer truth-table-buffer-name)
    (tabulated-list-mode)
    (cl-destructuring-bind (format . entries) truth-table-list
      (let ((spaces (list)))
        (setq tabulated-list-format
              (map 'vector
                   (lambda (item)
                     (let* ((s (format "%s" item))
                            (l (length s)))
                       (setq spaces (cons (cl-loop repeat (/ l 2) concat " ")
                                          spaces))
                       (list s l t)))
                   format))
        (setq spaces (nreverse spaces))
        (setq tabulated-list-entries
              (map 'list
                   (lambda (item)
                     (list nil (map 'vector (lambda (n s) (format "%s%d" s n))
                                    item spaces)))
                   entries))))

    (tabulated-list-init-header)
    (tabulated-list-print)))

(provide 'truth-table)
;;; truth-table.el ends here
