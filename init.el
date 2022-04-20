;; Inspired by Emacs from Scratch
;; https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ

(setq inhibit-startup-message t) ; Start with a blank screen

;; Tweaks UI to be ultra clean
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar

;; Completely turn off bells
(setq visible-bell nil)   ; Visual bell
(setq ring-bell-function 'ignore) ; Annoying sound bell

;; Make ESC quit stuffs
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Always UTF-8
(set-language-environment "UTF-8")

(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; [fixme] don't highlight lines over X chars -- caused by whitespace mode, not in effect after deleting it
;; whitespace mode interferes with org-mode variable font, it'll treat spaces in pitched font as variable font and produce very strange spacing.
;; [fixme] highlight the textwidth column

;; Always enable visual line (i.e., line wrap)
(global-visual-line-mode 1)

(setq-default show-trailing-whitespace t)
(dolist (hook '(special-mode-hook
		term-mode-hook
		comint-mode-hook
		elfeed-search-update-hook
		elfeed-show-mode-hook
		compilation-mode-hook
		minibuffer-setup-hook))
	(add-hook hook
	    (lambda () (setq show-trailing-whitespace nil))))


(use-package highlight-indent-guides)
(setq highlight-indent-guides-method 'column)
;; [fixme] Should load after whitespace mode
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Use evil mode first so I don't get lost...
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))



(use-package command-log-mode)

;; restart-emacs
(use-package restart-emacs
  :init
  (setq restart-emacs-restore-frames t))

(use-package minimap
  :init
  (setq minimap-minimum-width 15)
  (setq minimap-window-location 'right))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

;;
;; Font settings generic
;;

;; avoid problems with emacs running in terminal
(when (display-graphic-p)

    (set-face-attribute 'default nil :font "Sarasa Term SC" :height 130)

    ;; [fixme] work around for mixed-pitch-mode
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "Sarasa Term SC" :height 130)))

    (create-fontset-from-fontset-spec
    (font-xlfd-name
    (font-spec :family "ETBembo"
		:height 130
		:registry "fontset-myvariable")))

    (set-fontset-font
    "fontset-myvariable"
    'han (font-spec :family "FZPingXianYaSongS-R-GB" :height 130))

    (create-fontset-from-fontset-spec
    (font-xlfd-name
    (font-spec :family "Iosevka"
		:height 130
		:registry "fontset-mypitch")))

    (set-fontset-font
    "fontset-mypitch"
    'han (font-spec :family "Sarasa Term SC" :height 130))

    (set-face-attribute 'variable-pitch nil :fontset "fontset-myvariable" :font "fontset-myvariable" :height 130)

    (set-face-attribute 'fixed-pitch nil :fontset "fontset-mypitch" :font "fontset-mypitch" :height 130))

;;
;; Org Mode Configuration ------------------------------------------------------
;;

;; Configure org-mode
(defun cz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun cz/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1))))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil   :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook (org-mode . cz/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (cz/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

(defun cz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; [fixme] mixed-pitch mode doesn't work perfectly. It uses :family
;; and ignores :fontset. Leading to some undesirable results : (
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package visual-fill-column
  :hook (org-mode . cz/org-mode-visual-fill))

;;
;; == End of Org-mode setup
;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode))

;; [fixme] automated popups
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-material-dark t)

  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer cosmos/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Dynamically change font size via hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(cosmos/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun edit-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))


(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package all-the-icons
  :if (display-graphic-p))

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Custom keymappings
(cosmos/leader-keys
  "ed" (lambda() (interactive) (find-file user-init-file))
  "t" '(:ignore t :which-key "toggles")
  "p" 'projectile-command-map
  "tt" '(counsel-load-theme :which-key "choose theme")
  "bb" 'counsel-switch-buffer)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package projectile-ripgrep)

(use-package elfeed
  :bind
  ("C-x w" . elfeed))

;; [fixme] try to load read items
(use-package elfeed-protocol
  :ensure t
  :demand t
  :after elfeed
  :config
  (elfeed-protocol-enable)
  :custom
  (elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (elfeed-log-level 'debug)
  (elfeed-feeds (list
		 (list "ttrss+http://admin@192.168.2.130:181"
		       :api-url "http://admin@192.168.2.130:181"
		       :password (shell-command-to-string "gopass -o freshrss")))))

(defun elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun cz/elfeed-all-read-refresh()
  (interactive)
  (when (y-or-n-p "Really mark all items as read?")
    (elfeed-mark-all-as-read)
    (elfeed-serch-fetch nil)))

;; use seprate custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
