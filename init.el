;; Inspired by Emacs from Scratch
;; https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ

;; [fixme] set startup page with recently edited files and bookmarks
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

;; 中文简单测试
;; Font settings generic
(set-face-attribute 'default nil :font "Iosevka" :height 130)

(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "FZPingXianYaSongS-R-GB" :height 130)))


;; Make ESC quit stuffs
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Always UTF-8
(set-language-environment "UTF-8")

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
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

;; [fixme] don't highlight lines over X chars
;; [fixme] highlight the textwidth column

;; Always enable visual line (i.e., line wrap)
(global-visual-line-mode 1)

(global-whitespace-mode 1)
(setq whitespace-global-modes '(not org-mode))
;; newline char as ¬
(setq whitespace-display-mappings
      '((newline-mark 10 [172 10])))

(setq-default show-trailing-whitespace t)

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

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package command-log-mode)

;; restart-emacs
(use-package restart-emacs
  :init
  (setq restart-emacs-restore-frames t))

;; Org Mode Configuration ------------------------------------------------------
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
		  (org-level-8 . 1.1)))))

(use-package org
  :hook (org-mode . cz/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (cz/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cz/org-mode-visual-fill))

;; == End of Org-mode setup

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

;; 中文简单测试
;; setting up org-mode faces
;; src: https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 130))))
 '(fixed-pitch ((t (:family "Iosevka" :height 130 :style "regular")))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(custom-theme-set-faces
 'user
 ;; Also override white space, otherwise unable to achieve alignment as spaces are a different face
 '(whitespace-space ((t (:inherit fixed-pitch))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-describe-function-function 'helpful-callable)
 '(counsel-describe-variable-function 'helpful-variable)
 '(custom-safe-themes
   '("6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" default))
 '(doom-modeline-height 15)
 '(org-hide-emphasis-markers t)
 '(package-selected-packages
   '(undo-tree valign highlight-indent-guides cnfonts cnfont doom-themes restart-emacs elfeed elfeed-protocol org-plus-contrib projectile hydra general which-key use-package rainbow-delimiters ivy-rich helpful gruvbox-theme evil-surround evil-org evil-nerd-commenter evil-mu4e evil-magit evil-collection elfeed-org doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Iosevka" :height 130 :style "regular"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 130))))
 '(whitespace-space ((t (:inherit fixed-pitch)))))
