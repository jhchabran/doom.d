;; Running emacsclient in the terminal while having the gui app opened
;; can lead to have the toolbar to become visible. Duct tape this.
(defun jh/frame-tweaks (&optional frame)
  "Fix issues whem mixing terminal emacsclient and gui app at the same time"
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)))
    (menu-bar-mode -1)))

;; For the case that the init file runs after the frame has been created.
;; Call of emacs without --daemon option.
(jh/frame-tweaks)
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'jh/frame-tweaks t)

;; No current line highlight please
(setq global-hl-line-mode nil)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

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
