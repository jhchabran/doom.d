;;; Do not prompt me for which langage to HL when inserting code blocks
;;; in markdown.
(after! markdown-mode
  (setq markdown-gfm-use-electric-backquote nil))
