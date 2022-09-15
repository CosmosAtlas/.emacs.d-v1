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
(setq visible-bell nil)           ; Visual bell
(setq ring-bell-function 'ignore) ; Annoying sound bell

;; scrolling
(setq scroll-margin 2)
(global-hl-line-mode 1)

;; Always indent as spaces
(setq-default indent-tabs-mode nil)

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

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Always enable visual line (i.e., line wrap)
(global-visual-line-mode 1)

;; Highlight spaces at end of line
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


;;
;; My helper functions
;;
(defun cz/edit-user-init-file ()
  "Edit the `user-init-file'"
  (interactive)
  (find-file user-init-file))

(defun cz/edit-org-inbox-file ()
  "Edit the inbox file in org"
  (interactive)
  (find-file (file-truename (concat org-directory "/inbox.org"))))

(defun cz/edit-org-gtd-file ()
  "Edit the inbox file in org"
  (interactive)
  (find-file (file-truename (concat org-directory "/gtd.org"))))

;;
;; Configure packages {{{
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; End of package system setup }}}

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

;; auto alignment
(use-package evil-lion
  :custom
  (evil-lion-left-align-key (kbd "ga"))
  (evil-lion-right-align-key (kbd "gA"))
  :config
  (evil-lion-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; preview registers
(use-package evil-owl
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
	       '("*evil-owl*"
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (window-height . 0.3)))
  (evil-owl-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; End of evil

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'column)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

(use-package command-log-mode)

(use-package restart-emacs)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package no-littering
  :custom
  ;; may require restart or manually creating the dir for it to work
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Undo history
(use-package vundo)

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  ;; disable built-in autosave
  (setq auto-save-default nil))

;; Helper utilities
(use-package crux)

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
			(font-spec :family "Sarasa Term SC")))

    (create-fontset-from-fontset-spec
    (font-xlfd-name
     (font-spec :family "ETBembo"
		:registry "fontset-myvariable")))

    (set-fontset-font
    "fontset-myvariable"
    'han (font-spec :family "FZPingXianYaSong-R-GBK"))

    (create-fontset-from-fontset-spec
    (font-xlfd-name
     (font-spec :family "Iosevka"
		:registry "fontset-mypitch")))

    (set-fontset-font
    "fontset-mypitch"
    'han (font-spec :family "Sarasa Term SC"))

    (set-face-attribute 'variable-pitch nil :fontset "fontset-myvariable" :font "fontset-myvariable" :height 1.0)

    (set-face-attribute 'fixed-pitch nil :fontset "fontset-mypitch" :font "fontset-mypitch" :height 1.0)

    ;; Set larger default font-size on MacOS
    (if (eq system-type 'darwin) (set-face-attribute 'default nil :height 200)))

;;
;; Org Mode Configuration {{{
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
  (dolist (face '((org-level-1 . 1.3)
		  (org-level-2 . 1.2)
		  (org-level-3 . 1.1)
		  (org-level-4 . 1.05)
		  (org-level-5 . 1.05)
		  (org-level-6 . 1.05)
		  (org-level-7 . 1.05)
		  (org-level-8 . 1.05))))

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
  (org-mode . cz/org-font-setup)
  :custom
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files (list (concat org-directory "/gtd.org")
			  (concat org-directory "/misc.org")
			  (concat org-directory "/done.org")
			  (concat org-directory "/inbox.org")))
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))
  (org-capture-templates
   `(("i" "inbox" entry (file+headline ,(concat org-directory "/inbox.org") "Tasks") "** TODO %?")))
  (org-log-done 'time)
  :config
  (setq org-ellipsis " ▾")
  ;; Basically conseal in vim
  (setq org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-autolist
  :after org
  :hook (org-mode . org-autolist-mode))

;; [fixme] explore org-roam-ui
(use-package org-roam
  :after org
  :defer t
  :config
  (setq org-roam-directory
	(file-truename (concat org-directory "/org-roam-test")))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template "${title} [${tags}]")
  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ))
  ;; run sync at startup
  (org-roam-db-autosync-mode))

(use-package org-ref
  :after org org-roam
  :config
  (setq org-ref-default-biblography (list (file-truename (concat org-directory "/zotero.bib")))))

(use-package org-roam-bibtex
  :after org org-roam org-ref ivy-bibtex
  :hook
  (org-mode . org-roam-bibtex-mode)
  :custom
  (orb-insert-interface 'ivy-bibtex))

(setq orb-preformat-keywords '("citekey" "author" "date"))
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
	 :unnarrowed t)
	("r" "bibliography reference" plain "%?"
         :target
         (file+head "references/${citekey}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(defun cz/org-id-update-roam-locations ()
  (interactive)
  (org-id-update-id-locations (org-roam--list-files org-roam-directory)))

;; [fixme] mixed-pitch mode doesn't work perfectly. It uses :family
;; and ignores :fontset. Leading to some undesirable results : (
;; currently hacked by overloading default fontset
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(defun cz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook
  (org-mode . cz/org-mode-visual-fill))

;;
;; End of Org-mode setup }}}
;;

;; [fixme] spell check

(use-package markdown-mode
  :commands (markdown-mode gfm-mode))

;; [fixme] automated popups
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-<return>" . ivy-immediate-done)
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

(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy counsel
  :init
  (ivy-rich-mode 1))

(use-package ivy-bibtex
  :after ivy ivy-rich
  :custom
  (ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  :config
  (setq bibtex-completion-bibliography
   (list (file-truename (concat org-directory "/zotero.bib")))))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
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
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC f") 'link-hint-open-link))

(use-package general
  :config
  (general-create-definer cz/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Zooming emacs frame globally
(straight-use-package 'frame-fns)
(require 'frame-fns)
(straight-use-package 'frame-cmds)
(require 'frame-cmds)
(straight-use-package 'zoom-frm)
(require 'zoom-frm)

;; Dynamically change font size via hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" zoom-in "in")
  ("k" zoom-out "out")
  ("r" (zoom-in/out 0) "reset")
  ("f" nil "finished" :exit t))

(cz/leader-keys
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
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package projectile-ripgrep)

(use-package elfeed
  :bind
  ("C-x w" . elfeed))

(use-package elfeed-protocol
  :after elfeed
  :config
  (setq elfeed-use-curl t)
  (setq elfeed-set-timeout 36000)
  (setq elfeed-curl-extra-arguments '("--insecure"))
  (setq elfeed-protocol-log-trace t)
  (setq elfeed-log-level 'debug)
  (setq elfeed-protocol-fever-update-unread-only t)
  (setq elfeed-feeds '(("ttrss+http://admin@192.168.1.104:181"
		   :api-url "http://admin@192.168.1.104:181"
		   :password (shell-command-to-string "gopass -o freshrss"))
		  ("fever+https://cosmos@cosmos547.com/miniflux/"
		   :api-url "https://cosmos@cosmos547.com/miniflux/fever/"
		   :password (shell-command-to-string "gopass -o hyperion"))))
  (setq elfeed-protocol-enabled-protocols '(fever ttrss))
  (elfeed-protocol-enable))

(use-package ebuku
  :after evil-collection org
  :custom
  (ebuku-database-path (file-truename (concat org-directory "/bookmarks.db"))))

(use-package fanyi)

;;
;; autocompletion setup
;;

(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
	      ("RET" . nil))
  :config
  (setq corfu-auto t
	corfu-quit-no-match 'separator))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

;; Enhance the ordeing of things
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-override '((file (styles basic partial-completion)))))

;;
;; Programming Setup
;;

;; Lua
(use-package lua-mode)

;; Common lisp
(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(defun cz/override-slime-del-key ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

(use-package paredit
  :after slime
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interactive-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'cz/override-slime-del-key))


;; LSP related
(use-package eglot
  :hook
  (python-mode-hook . eglot-ensure))

;; LaTeX

;; Don't bother with PDF tools on windows
(when (not (eq system-type 'windows-nt))
  (use-package pdf-tools
    :config
    (pdf-tools-install)))

(use-package tex
  :straight auctex
  :config
  (setq LaTeX-item-indent 0)
  (setq TeX-PDF-mode t)
  (eval-after-load
      "tex" '(add-to-list 'TeX-command-list
			  '("latexmk" "latexmk -pdf %t --synctex=1" TeX-run-TeX)))

  ;; Don't bother with PDF tools on windows
  (when (not (eq system-type 'windows-nt))
    (eval-after-load
        "tex" '(add-to-list 'TeX-view-program-selection
			    '(output-pdf "PDF Tools"))))

  (setq latex-run-command "pdflatex")
  (setq LaTeX-command "latex --synctex=1")
  (setq-default TeX-output-dir "build")
  (setq-default TeX-master nil)

  (setq TeX-auto-save t
	TeX-parse-self t
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (add-hook 'LaTeX-mode-hook #'(lambda () (reftex-mode))))

(use-package gscholar-bibtex)

;;
;; Finalizing settings after plugins
;;

;; Custom keymappings
(cz/leader-keys
  ;; editor behavior
  "bb" 'counsel-switch-buffer
  "bk" 'kill-this-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer
  ;; file edit short cuts
  "ed" 'cz/edit-user-init-file
  "eti" 'cz/edit-org-inbox-file
  "etg" 'cz/edit-org-gtd-file
  ;; launch apps
  "xg" 'magit
  "xe" 'ebuku
  "xr" 'elfeed
  ;; projectile
  "p" 'projectile-command-map
  ;; vundo
  "ut" 'vundo
  ;; zettelkasten related
  "zf" 'org-roam-node-find
  "zc" 'org-roam-capture
  ;; toggles
  "t" '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  ;; global org
  "oa" 'org-agenda
  "oc" 'org-capture)

(evil-define-key 'normal 'org-mode-map " ob" 'ivy-bibtex)
(evil-define-key 'normal 'org-mode-map " zi" 'org-roam-node-insert)
(evil-define-key 'normal 'org-mode-map " zb" 'org-roam-buffer-toggle)
(evil-define-key 'normal 'org-mode-map " zt" 'org-roam-tag-add)

;; load from custom files
(setq custom-file "~/.emacs.d/custom.el")
;; create the custom file if it does not exist
(write-region "" nil custom-file)
(load custom-file)
