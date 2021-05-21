;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Attempt at fixing spinner of death
(setq gcmh-high-cons-threshold (* 16 1024 1024))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "JH Chabran"
      user-mail-address "jh@chabran.fr")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 15))

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-oceanic-next)

(setq doom-theme 'doom-monarized)
(setq doom-monarized-brighter-comments t)

;; Use both explicit tags and the last directory to tag roam notes
;; (setq org-roam-tag-sources '(prop last-directory))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Projectile project search path
(setq projectile-project-search-path "~/code/src/github.com/jhchabran")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; I prefer SPC SPC as my local leader
(setq doom-localleader-key "SPC SPC")
(setq doom-localleader-alt-key "M-SPC SPC")

(map!
 :leader
 ;; clear existing binding, I use C-w all the time instead
 "SPC" nil
 :desc "Save current buffer" "w" #'save-buffer
 (:prefix-map ("b" . "buffer")
  :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer)
 (:prefix-map ("p" . "project")
  :desc "Launch term with custom name" "$" #'jh/launch-term-and-rename)
 (:prefix-map ("p" . "project")
  :desc "Open notes for the current project" "n" #'jh/open-project-notes)
 (:prefix-map ("c" . "code")
  :desc "Jump to local symbol" "c" #'ivy-lsp-current-buffer-symbols-jump
  :desc "Glance at the doc" "d" #'lsp-ui-doc-glance)) ;; go to def is bound to gd anyway


(map! :map evil-window-map "SPC" #'ace-swap-window)
(map! :map evil-normal-state-map "TAB" #'evil-forward-arg)

;; I don't use this, so let's turn it off
(after! evil-escape (evil-escape-mode -1))

;; Is there a case where you don't want the substitutions
;; to be global?
(after! evil (setq evil-ex-substitute-global t))

;; (setq-default left-margin-width 2 right-margin-width 2)
;; (set-window-buffer nil (current-buffer))
(setq scroll-margin 10)

;; tty mouse scrolling until https://github.com/hlissner/doom-emacs/issues/4137 is fixed
(map! :unless (display-graphic-p)
      :nvi "<mouse-4>" (cmd! (scroll-down 1))
      :nvi "<mouse-5>" (cmd! (scroll-up 1)))


;; I use a colemak scheme, so let's reflect that
(after! switch-window
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "t" "d" "h" "n" "e" "e" "o")))

;; https://github.com/hlissner/doom-emacs/issues/1530
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))
(defun go-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'golangci-lint))
(add-hook 'go-mode-lsp-hook
          #'go-flycheck-setup )

;; https://github.com/golang/go/issues/32394
(after! lsp-mode
  ;; (setq lsp-go-gopls-server-args '("-logfile" "gopls.log" "-rpc.trace") )
  ;; Not yet released in emacs-lsp
  ;; (setq lsp-go-use-gofumpt t)
  (lsp-register-custom-settings
   '(("gopls.experimentalWorkspaceModule" t t)
     ("gopls.semanticTokens" t t)
     ("gopls.experimentalPostfixCompletions" t t))))

;; (after! lsp-mode
;;     (setq lsp-go-codelenses '(gc_details generate tidy)))
;; Javascript
(setq js2-basic-offset 2)

(use-package! dap-mode)


;; custom faces to show breakpoints when in text ui
(custom-set-faces
 '(dap-ui-pending-breakpoint-face ((t (:foreground "#d33682"))))
 '(dap-ui-verified-breakpoint-face ((t (:background "#d33682" :foreground "#13383C")))))

;; My own stuff
(require 'ivy-lsp-current-buffer-symbols)

;; When I want to prevent myself from committing something, I enter the NO COMMIT comment (without space, otherwise it'll trigger the githook here)
(after! hl-todo
  (push `(,(concat "NO" "COMMIT") error bold) hl-todo-keyword-faces))


(setq haskell-process-type 'stack-ghci)

;; (defun +go/test-single-table (
;;   (interactive)
;;   (if (string-match "_test\\.go" buffer-file-name)
;;       (+go--run-tests buffer-file-name)
;;     (error "Must be in a _test.go file")) )

(defun +go/test-file ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (+go--run-tests buffer-file-name)
    (error "Must be in a _test.go file")))

(map! :map go-mode-map
      :localleader
      (:prefix ("t" . "test")
       "f" #'+go/test-file))

;; (defun jh/treemacs-cosmetic-dir (name)
;;   (concat "▹ " name))

;; (setq treemacs-directory-name-transformer #'jh/treemacs-cosmetic-dir)

(after! dap-mode
  ;; I rarely want more than this
  (setq dap-auto-configure-features '(breakpoints locals)))

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
                                                        (s-replace "\n" "\\n" (replace-regexp-in-string "github\.com/genjidb/genji/" "♠" value)))
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

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)))
    (menu-bar-mode -1)))

;; For the case that the init file runs after the frame has been created.
;; Call of emacs without --daemon option.
(my-frame-tweaks)
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)


;; No current line highlight please
(setq global-hl-line-mode nil)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;;; Do not prompt me for which langage to HL when inserting code blocks
;;; in markdown.
(setq markdown-gfm-use-electric-backquote nil)

;; Some tweaks taken from Nicolas Rougier Elegant emacs,
;; https://github.com/rougier/elegant-emacs.
(setq default-frame-alist
      (append (list
               '(min-height . 1)  '(height     . 45)
               '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 21)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

;; Vertical window divider
(setq window-divider-default-right-width 20)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(load! "+functions")
(load! "+org")
(load! "+irc")
