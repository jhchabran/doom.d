(defun circe-command-ZNC (what)
  "Send a message to ZNC incorporated by user '*status'."
  (circe-command-MSG "*status" what))

(defun jh/auth-server-pass (server)
  (if-let ((secret (plist-get (car (auth-source-search :host server)) :secret)))
      (if (functionp secret)
          (funcall secret) secret)
    (error "Could not fetch password for host %s" server)))

(defun register-irc-auths ()
  (require 'circe)
  (require 'dash)
  (let ((accounts (-filter (lambda (a) (string= "irc" (plist-get a :for)))
                           (auth-source-search :require '(:for) :max 10))))
    (appendq! circe-network-options
              (mapcar (lambda (entry)
                        (let* ((host (plist-get entry :host))
                               (label (or (plist-get entry :label) host))
                               (_ports (mapcar #'string-to-number
                                               (s-split "," (plist-get entry :port))))
                               (port (if (= 1 (length _ports)) (car _ports) _ports))
                               (user (plist-get entry :user)))
                          `(,label
                            :host ,host :port ,port
                            :pass jh/auth-server-pass)))
                      accounts))))

(defun jh/irc ()
  (interactive)
  (+workspace-switch "IRC" t)

  (circe "gnome.local"))

(after! circe
  (custom-set-faces!
    '(circe-my-message-face :foreground nil 'highlight))

  (disable-lui-logging-globally)
  (enable-circe-display-images)

  (defun named-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (format "%13s > " (circe-nick))
                         'face 'circe-prompt-face)
             "")))
  (add-hook 'circe-chat-mode-hook #'named-circe-prompt))

(add-transient-hook! #'jh/irc (register-irc-auths))
