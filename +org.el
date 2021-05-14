;; Where the files are stored. I sync those in a Dropbox account and use an iOS app to
;; access them on the go.
(setq org-directory "~/org/")

;; TODO items can be stored in any of my files.
;; (setq org-agenda-files
;;       (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-files
      (mapcar 'file-truename
        (file-expand-wildcards "~/org/*.org")))

;; Mix fixed width and normal fonts in org files
(add-hook! 'org-mode-hook #'mixed-pitch-mode)

;; TODO I don't really remember what this does.
(setq mixed-pitch-set-height t)


;; Some eye candy and usability things.
(after! org
  (setq
   org-hide-emphasis-markers t
   org-insert-heading-respect-content nil
   org-src-tab-acts-natively t))

;; Agenda format
(use-package! org-agenda
  :after org
  :custom
  (org-agenda-prefix-format '((agenda . " %i %-20:c%?-12t%-6e% s")
                              (todo   . " %i %-20:c %-6e")
                              (tags   . " %i %-20:c")
                              (search . " %i %-20:c"))))

;; Pretty classic, with the addition of goals being displayed before everything
;; else.
(setq org-agenda-custom-commands
      '(("d" "Today's Tasks"
         ((tags-todo
           "GOAL+ACTIVE+PRIORITY=\"A\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Primary goals this month")))
          (tags-todo
           "GOAL+ACTIVE+PRIORITY=\"C\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Secondary goals this month")))
          (agenda "" ((org-agenda-span 1)
                      (org-agenda-overriding-header "Today")))))

        ("w" "This Week's Tasks"
         ((tags-todo
           "GOAL+ACTIVE+PRIORITY=\"A\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Primary goals this month")))
          (tags-todo
           "GOAL+ACTIVE+PRIORITY=\"C\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Secondary goals this month")))
          (agenda)))
        ("g" "GTD"
         ((tags-todo
           "GOAL+ACTIVE+PRIORITY=\"A\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Primary goals this month")))
          (tags-todo
           "GOAL+ACTIVE+PRIORITY=\"C\""
           ((org-agenda-files '("~/org/goals.org"))
            (org-agenda-overriding-header "Secondary goals this month")))
          (agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

;; Remove those horrible ====== separators
(setq org-agenda-block-separator (string-to-char " "))

(after! org-roam
  ;; Do not display the sidebar
  (setq +org-roam-open-buffer-on-find-file nil)
  ;; TODO why do I need this?
  (add-hook 'org-roam-mode-hook (lambda ())
            (variable-pitch-mode 1)))

(setq-hook! 'web-mode-hook +format-with 'prettier-prettify)

;; The display of the task list is bit redundant because we have an “inbox” on
;; the left and an “inbox” on the right. However, they do not have the same origin.
;; The one on the left is the name of the file where the related TODO is
;; stored while the one on the right is a tag. If you remember our inbox file setup header,
;; there was a #+FILETAGS: inbox line that assign the “inbox” tag to each entry.
;; Since tags are redundant, let’s just remove them by filtering them out:
(setq org-agenda-hide-tags-regexp ".")

;; Templates
(setq org-capture-templates
      `(("i" "Todo" entry (file "inbox.org")
         ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("t" "Todo with link" entry (file "inbox.org")
         ,(concat "* TODO %?\n" "%l\n" "/Entered on/ %U"))
        ("m" "Meeting" entry (file+headline "agenda.org" "Future")
         ,(concat "* %? :meeting:\n" "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "notes.org")
         ,(concat "* Note (%a)\n" "/Entered on/ %U\n" "\n" "%?"))))

(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))


(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; This will add a CLOSED: [2020-09-13 Sun 19:24] line under the corresponding entry.
(setq org-log-done 'time)

;; Save the corresponding buffers when refiling
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ())
       (when (member (buffer-file-name) org-agenda-files)
         t))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
      (lambda (&rest _)
        (gtd-save-org-buffers)))

(defun jh/capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun jh/capture-inbox-link ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "t"))

(defun jh/agenda ()
  (interactive)
  ;; autocreate the agenda workspace
  (+workspace-switch "agenda" t)
  (org-agenda nil "g"))

(map!
 :leader
 "SPC" nil
 (:prefix-map ("a" . "agenda")
  :desc "Capture (inbox)" "i" #'jh/capture-inbox
  :desc "Capture w/ Link (inbox)" "t" #'jh/capture-inbox-link
  :desc "Agenda" "a" #'jh/agenda))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-document-title ((t (:inherit outline-1 :heigth 1.3)))))

;; I don't like christmas tree org files, gimme simple bullets!
(setq org-superstar-headline-bullets-list '("*"))
