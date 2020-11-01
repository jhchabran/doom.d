;;; +functions.el -*- lexical-binding: t; -*-

(defun jh/launch-term-and-rename ()
  (interactive)
  (let* ((name (read-string "Enter buffer NAME (will be named *term-NAME*): ")))
        (vterm (concat "term-" name))))
