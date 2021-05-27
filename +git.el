(use-package! magit-delta
  :after magit
  :commands magit-delta-mode
  :custom
  (magit-delta-default-dark-theme "none")
  :hook (magit-mode . magit-delta-mode))
