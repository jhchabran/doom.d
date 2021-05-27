;; https://github.com/hlissner/doom-emacs/issues/1530
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))
(defun go-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'golangci-lint))
(add-hook 'go-mode-lsp-hook
          #'go-flycheck-setup)

(after! lsp-ui
  (setq lsp-ui-doc-max-width 60))


(after! lsp-mode
  ;; trace lsp stuff when debugging
  ;; (setq lsp-go-gopls-server-args '("-logfile" "gopls.log" "-rpc.trace") )
  ;; Not yet released in emacs-lsp
  ;; (setq
  (setq
   ;; not yet released
   ;; lsp-go-use-gofumpt t
   lsp-go-codelenses '(gc_details generate tidy))

  (lsp-register-custom-settings
   ;; https://github.com/golang/go/issues/32394
   '(("gopls.experimentalWorkspaceModule" t t)
     ("gopls.semanticTokens" t t)
     ("gopls.experimentalPostfixCompletions" t t))))

(require 'ivy-lsp-current-buffer-symbols)

;; mappings
(map!
 :leader
 (:prefix-map ("c" . "code")
  :desc "Jump to local symbol" "c" #'ivy-lsp-current-buffer-symbols-jump
  :desc "Glance at the doc" "d" #'lsp-ui-doc-glance)) ;; go to def is bound to gd anyway
