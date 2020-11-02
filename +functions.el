;;; +functions.el -*- lexical-binding: t; -*-

(defun jh/launch-term-and-rename ()
  (interactive)
  (let* ((name (read-string "Enter buffer NAME (will be named *term-NAME*): ")))
        (vterm (concat "term-" name))))

(defun jh/open-project-notes ()
  (interactive)
  (let* ((name (print (file-name-nondirectory (projectile-project-root))))
         (name (car (reverse (butlast (split-string (projectile-project-root) "/") 1))))
         (path (concat org-directory "/projects/" name ".org")))
    (find-file path)))
