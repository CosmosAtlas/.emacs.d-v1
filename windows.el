;; Settings specific to windows

;; org directory to following standard $HOME instead of AppData...
(setq org-directory (format "C:/Users/%s/org" user-login-name))

(setq w32-allow-system-shell t)

;; (set-default 'process-coding-system-alist
;;              '(("buku" . (utf-8 . gbk-dos))
;; 	       ("[cC][mM][dD][pP][rR][oO][xX][yY]" . (utf-8 . gbk-dos))))

;; To verify that calling a program uses the correct encoding, use the following
;; (find-operation-coding-system 'call-process "buku")
