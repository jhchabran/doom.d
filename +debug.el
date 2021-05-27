(use-package! dap-mode)

(after! dap-mode
  ;; I rarely want more than this
  (setq dap-auto-configure-features '(breakpoints locals)))

;; TODO is there a better way to do this? defadvice maybe?
;; Horrible patch, but does the job
(defun dap-ui-render-variables (debug-session variables-reference _node)
  "Render hierarchical variables for treemacs.
Usable as the `treemacs' :children argument, when DEBUG-SESSION
and VARIABLES-REFERENCE are applied partially.
DEBUG-SESSION specifies the debug session which will be used to
issue requests.
VARIABLES-REFERENCE specifies the handle returned by the debug
adapter for acquiring nested variables and must not be 0."
  (when (dap--session-running debug-session)
    (->>
        variables-reference
      (dap-request debug-session "variables" :variablesReference)
      (gethash "variables")
      (-map (-lambda ((&hash "value" "name"
                             "variablesReference" variables-reference))
              `(:label ,(concat (propertize (format "%s" name)
                                            'face 'font-lock-variable-name-face)
                                ": "
                                (propertize (s-truncate dap-ui-variable-length
                                                        (s-replace "\n" "\\n" (replace-regexp-in-string "github\.com/" "⃨" value)))
                                            'help-echo value))
                :icon dap-variable
                :value ,value
                :session ,debug-session
                :variables-reference ,variables-reference
                :name ,name
                :actions '(["Set value" dap-ui-set-variable-value])
                :key ,name
                ,@(unless (zerop variables-reference)
                    (list :children
                          (-partial #'dap-ui-render-variables debug-session
                                    variables-reference)))))))))
