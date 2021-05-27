;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Attempt at fixing spinner of death
(setq gcmh-high-cons-threshold (* 16 1024 1024))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "JH Chabran"
      user-mail-address "jh@chabran.fr")

(setq doom-font (font-spec :family "JetBrains Mono Light" :size 14)
      doom-big-font (font-spec :family "JetBrains Mono Light" :size 18)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 16))

(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)

(setq doom-theme 'doom-monarized)
(setq doom-monarized-brighter-comments t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Projectile project search path
(setq projectile-project-search-path "~/code/src/github.com/jhchabran")

;; I prefer SPC SPC as my local leader
(setq doom-localleader-key "SPC SPC")
(setq doom-localleader-alt-key "M-SPC SPC")

(map!
 :leader
 ;; clear existing window binding, C-w is really hardwired in my brain anyway
 "SPC" nil
 :desc "Save current buffer" "w" #'save-buffer
 (:prefix-map ("b" . "buffer")
  :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer)
 (:prefix-map ("p" . "project")
  :desc "Launch term with custom name" "$" #'jh/launch-term-and-rename)
 (:prefix-map ("p" . "project")
  :desc "Open notes for the current project" "n" #'jh/open-project-notes))

(map! :map evil-window-map "SPC" #'ace-swap-window)
(map! :map evil-normal-state-map "TAB" #'evil-forward-arg)

;; I don't use stuff like jk to go back to normal mode, so let's turn it off.
(after! evil-escape (evil-escape-mode -1))

;; Is there a case where you don't want the substitutions
;; to be global?
(after! evil (setq evil-ex-substitute-global t))

(setq scroll-margin 10)

;; tty mouse scrolling until https://github.com/hlissner/doom-emacs/issues/4137 is fixed
(map! :unless (display-graphic-p)
      :nvi "<mouse-4>" (cmd! (scroll-down 1))
      :nvi "<mouse-5>" (cmd! (scroll-up 1)))

;; I use a colemak scheme, so let's reflect that
(after! switch-window
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "t" "d" "h" "n" "e" "e" "o")))

;; When I want to prevent myself from committing something, I enter the NO COMMIT
;; comment (without space, otherwise it'll trigger the githook here)
(after! hl-todo
  (push `(,(concat "NO" "COMMIT") error bold) hl-todo-keyword-faces))


(load! "+functions")
(load! "+git")
(load! "+org")
(load! "+lsp")
(load! "+irc")
(load! "+cosmetic")
(load! "+go")
(load! "+haskell")
(load! "+markdown")
(load! "+js")

;; temp fix https://discord.com/channels/406534637242810369/406554085794381833/841224052919107594
(defadvice! shut-up-format-enable-on-save-h (orig-fn &rest args)
  :around #'+format-enable-on-save-h
  (unless (memq (car (format-all--probe)) '(:none nil))
    (apply orig-fn args)))

;; https://gitlab.com/zzamboni/dot-doom/-/tree/master/splash
(setq fancy-splash-image (concat doom-private-dir "splash/doom-emacs-color.png"))
