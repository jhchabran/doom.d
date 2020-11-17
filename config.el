;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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

(setq doom-font (font-spec :family "IBM Plex Mono" :size 14)
      doom-big-font (font-spec :family "IBM Plex Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 18))

;; Bold is often too heavy for my taste
(setq doom-themes-enable-bold nil)
(setq doom-themes-enable-italic nil)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-oceanic-next)
(setq doom-theme 'doom-nuit-dark)
(setq doom-nuit-dark-brighter-comments t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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
  :desc "Jump to local symbol" "c" #'ivy-lsp-current-buffer-symbols-jump))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(setq mixed-pitch-set-height t)

(after! org (setq
             org-hide-emphasis-markers t
             org-insert-heading-respect-content nil
             org-src-tab-acts-natively t))

(add-hook 'text-mode-hook (lambda ()
                            (setq left-margin-width 8
                                  right-margin-width 8)))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  '(org-document-title ((t (:inherit outline-1 :heigth 1.5)))))

(after! org-roam
  (add-hook 'org-roam-mode-hook (lambda ())
                             (variable-pitch-mode 1)))

;; I use a colemak scheme, so let's reflect that
(after! switch-window
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "t" "d" "h" "n" "e" "e" "o")))

;; (plist-put +ligatures-extra-symbols
;;            '(:not-equal "≠"))


(after! go-mode
  (set-ligatures! 'go-mode
    :lambda "func"
    ;; :true "true" :false "false"
    :null "nil"
    ;; :str "string"
    :yield "select"
    :return "return"
    :and "&&"
    :or "||"
    :not-equal "!="
    :for "for"))

;; Javascript
(setq js2-basic-offset 2)

;; My own stuff
(require 'ivy-lsp-current-buffer-symbols)

(load! "+functions")
(load! "themes/doom-nuit-dark-theme")

