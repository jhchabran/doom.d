;;; +functions.el -*- lexical-binding: t; -*-

(defun jh/launch-term-and-rename ()
  (interactive)
  (let* ((name (read-string "Enter buffer NAME (will be named *term-NAME*): ")))
        (vterm (concat "term-" name))))

(defun jh/open-project-notes ()
  (interactive)
  (let* ((name (car (reverse (butlast (split-string (projectile-project-root) "/") 1))))
         (path (concat org-directory "/roam/" name ".org")))
    (find-file path)))


