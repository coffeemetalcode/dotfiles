;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package repo management;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end package repo management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; install packages ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package command-log-mode
  :ensure t)

(use-package counsel
  :ensure t
  :init (ivy-mode 1))

(use-package ivy
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 35)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.75))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :ensure t)
(global-company-mode)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Workspace/coffeemetalcode")
    (setq projectile-project-search-path '("~/Workspace/coffeemetalcode")))
  (setq projectile-switch-project-action #'projectile-dired))
;;  (add-to-list 'projectile-globally-ignored-directories "node_modules")

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(with-eval-after-load 'magit-mode
  (add-hook
   'after-save-hook
   'magit-after-save-refresh-status t))

(use-package treemacs
  :ensure t
  :bind
  ;; think about these keybindings
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; ligatures for fira code
;; instructions: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;; requires Fira Code Symbol font in addition to Fira Code Regular
(use-package fira-code-mode
  :ensure t
  :if (display-graphic-p)
  :custom
  ;; some ligatures are the opposite of helpful
  (fira-code-mode-disabled-ligatures '("[]"))
  :hook prog-mode)

;; nerd icons
(use-package nerd-icons
  :ensure t)

;; nerd icons for treemacs
(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-nerd-icons-config))

;; github copilot
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))



(use-package indent-bars
  :ensure t
  :hook ((emacs-lisp-mode
	  markdown-mode
	  typescript-mode
	  ng2-mode
	  web-mode
	  json-mode
	  js2-mode) . indent-bars-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end install packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; look and feel ;;
;;;;;;;;;;;;;;;;;;;

;; (menu-bar-mode -1)    ;; supress menubar (i.e. 'File', 'Edit', etc.)
(tool-bar-mode -1)    ;; supress toolbar (i.e. 'New File', 'Cut', etc.)
(scroll-bar-mode -1)  ;; disable vertical scroll bars
(setq visible-bell t) ;; set visual bell
(setopt display-fill-column-indicator-column 80) ;; set line at 80 chars
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode) ;; and display
(set-face-attribute 'default nil
		    :font "Fira Code Medium"
		    :height 100)
(electric-pair-mode t) ;; automatically close brackets
(load-theme 'doom-one t)
(column-number-mode) ;; what does this do?
(global-display-line-numbers-mode 0) ;; line numbers managed in prog-mode-hook
(setq-default truncate-lines t) ;; don't wrap lines
(command-log-mode 1) ;; to see, do M-x clm/toggle-command-line-buffer
(setq warning-minimum-level :error)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(set-face-attribute 'treemacs-window-background-face nil
		    :family "Nunito Medium"
		    :height 100)
(treemacs-indent-guide-mode)
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; buffer tabs
(global-tab-line-mode 1)
;; detect when buffers are changed via a process (like 'git pull' or 'npm i')
(global-auto-revert-mode 1)
;; detect when git status changes
(magit-auto-revert-mode 1)
;; don't make backup files i.e. 'file~'
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;
;; end look and feel ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; lsp mode ;;
;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (setq lsp-headerline-breadcrumb-enable nil)
  :config
  (lsp-enable-which-key-integration t))

;;;;;;;;;;;;;;;;;;
;; end lsp mode ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; prog modes ;;
;;;;;;;;;;;;;;;;

;; treemacs
(use-package lsp-treemacs
  :ensure t)
(desktop-save-mode 1) ;; save the current desktop, including treemacs state

;; typescript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; angular typescript
(use-package ng2-mode
  :ensure t
  :hook (ng2-mode . lsp-deferred)
  :config
  ;; Associate ng2-mode with Angular component files
  (add-to-list 'auto-mode-alist '("\\.component\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . ng2-html-mode))
  (add-to-list 'auto-mode-alist '("\\.service\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.module\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.directive\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pipe\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.guard\\.ts\\'" . ng2-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.resolver\\.ts\\'" . ng2-ts-mode)))

;; html and css
(use-package web-mode
  :ensure t
  :hook (web-mode . lsp-deferred)
  :mode ("\\.html\\'" "\\.css\\'" "\\.scss\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

;; json
(use-package json-mode
  :ensure t
  :hook (json-mod . lsp-deferred)
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2)
  (setq js-indent-level 2))

;;;;;;;;;;;;;;;;;;;;
;; end prog modes ;;
;;;;;;;;;;;;;;;;;;;;

;; below is auto-generated and auto-updated by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171"
     default))
 '(package-selected-packages
   '(atom-one-dark-theme command-log-mode company copilot copilot-chat
			 counsel-projectile doom-modeline doom-themes
			 emmet-mode exec-path-from-shell
			 fira-code-mode flycheck helpful indent-bars
			 ivy-rich json-mode lsp-treemacs lsp-ui magit
			 minimap ng2-mode projectile-ripgrep
			 rainbow-delimiters spacious-padding
			 treemacs-nerd-icons web-mode which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
