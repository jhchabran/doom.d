(defun +go/test-file ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (+go--run-tests buffer-file-name)
    (error "Must be in a _test.go file")))

(map! :map go-mode-map
      :localleader
      (:prefix ("t" . "test")
       "f" #'+go/test-file))
