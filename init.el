;; Inspired by Emacs from Scratch
;; https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ

;;
;; Startup configurations
;;

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

;;
;; Per system specific configurations
;;

(load-file (expand-file-name
	    (cond ((eq system-type 'windows-nt) "windows.el")
		  ((eq system-type 'gnu/linux) "linux.el")
		  (t "default-system.el"))
	    user-emacs-directory))

;;
;; General configurations
;; -- Configurations here are built-in emacs
;;

;; Make ESC quit stuffs
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Always UTF-8
(set-language-environment "UTF-8")

;; manage temp files centrally
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves/"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

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


(defun cz/edit-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))

;;
;; Configure packages
;;

;; Set up package systems
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

;; auto update packages
(use-package auto-package-update
  :config
  (setq auto-package-update-deleted-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; End of package system setup

;;
;; List of packages and configurations
;;

;;
;; Configure Evil Mode
;;
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

;; End of evil

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

(use-package command-log-mode)

(use-package restart-emacs)

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
;; Org Mode Configuration
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
  :hook
  (org-mode . cz/org-mode-setup)
  (auto-save-hook . org-save-all-org-buffers)
  :custom
  (org-default-notes-file (concat org-directory "/notes.org"))
  :config
  (setq org-ellipsis " ▾")
  ;; Basically conseal in vim
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

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (concat org-directory "/org-roam-test")))
  :config
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

;; [fixme] mixed-pitch mode doesn't work perfectly. It uses :family
;; and ignores :fontset. Leading to some undesirable results : (
;; currently hacked by overloading default fontset
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package visual-fill-column
  :hook (org-mode . cz/org-mode-visual-fill))

(with-eval-after-load "ispell"
  (setq ispell-program-name "aspell")
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC")))

(add-hook 'org-mode-hook (lambda () (setq flyspell-mode 1)))

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
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-material-dark t)
  (doom-themes-org-config))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.3))

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
  ("C-c C-d" . helpful-at-point)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package link-hint
  :ensure t
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC f") 'link-hint-open-link))

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

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package all-the-icons
  :if (display-graphic-p))

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
  (elfeed-feeds '(("fever+http://cosmos@192.168.2.130:50180"
		   :api-url "http://cosmos@192.168.2.130:50180/api/fever.php"
		   :password (shell-command-to-string "gopass -o freshrss")))))

;;
;; autocompletion setup
;;

(use-package company
  :init
  (global-company-mode)
  :hook
  (after-init-hook . global-company-mode))

;; Enhance the ordeing of things
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-override '((file (styles basic partial-completion)))))

;;
;; Finalizing settings after plugins
;;

;; Custom keymappings
(cosmos/leader-keys
  "ed" 'cz/edit-user-init-file
  "t" '(:ignore t :which-key "toggles")
  "p" 'projectile-command-map
  "of" 'org-roam-node-find
  "oc" 'org-roam-capture
  "oi" 'org-roam-node-insert
  "tt" '(counsel-load-theme :which-key "choose theme")
  "bb" 'counsel-switch-buffer)

;; load from custom files
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
