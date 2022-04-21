;; Settings specific to windows

(setq org-directory (format "C:/Users/%s/org" user-login-name))
(setq org-default-notes-file (concat org-directory "/notes.org"))


;; temp for testing
(make-directory (concat org-directory "/org-roam-test"))
(setq org-roam-directory (file-truename (concat org-directory "/org-roam-test")))

(org-roam-db-autosync-mode)
