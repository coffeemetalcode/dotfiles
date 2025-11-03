;; package repo management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-pacakge))

(require 'use-package)
(setq use-package-alwasy-ensure t)

;;
;; end package repo management
;;

;;
;; install packages
;;

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

(use-package all-the-icons
  :ensure t)

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

(use-package counsel-projectile
  :ensure t
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :ensure t
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :ensure t
  :bind
  ;; think about these keybindings
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package atom-one-dark-theme
  :ensure t)

;;
;; end install packages
;;

;;
;; look and feel
;;

(menu-bar-mode -1)    ;; supress menubar (i.e. 'File', 'Edit', etc.)
(scroll-bar-mode -1)  ;; disable vertical scroll bars
(setq visible-bell t) ;; set visual bell
(set-face-attribute 'default nil
		    :font "FiraCode Nerd Font Light"
		    :height 115)
(electric-pair-mode t)
(load-theme 'atom-one-dark t)
(column-number-mode)
(global-display-line-numbers-mode 1)
(setq-default truncate-lines t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
(command-log-mode 1)
(setq warning-minimum-level :error)

;;
;; end look and feel
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(atom-one-dark-theme atom-one-dark all-the-icons treemacs magit counsel-projectile projectile company helpful ivy-rich which-key rainbow-delimiters doom-modeline doom-themes counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
