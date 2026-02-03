;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq doom-modeline-height 35)
(setq doom-modeline-buffer-file-name-style 'file-name)

(setq frame-title-format '("%b – " (:eval
                                    (projectile-project-name)) " – Doom Emacs"))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil) ;; use spaces instead of tabs

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)

(setq doom-font (font-spec
                 :family "Fira Code"
                 :size 15)
      doom-variable-pitch-font (font-spec
                                :family "Lato"
                                :size 15))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  ;; indentation settings
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  :bind (:map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)
         ("<backtab>" . 'copilot-clear-overlay)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         ("C-TAB" . 'copilot-accept-completion-by-word)))

(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-auto-enabled t
        ;; highlight-indent-guides-auto-odd-face-perc 80
        ;; highlight-indent-guides-auto-even-face-perc 100
        ;; highlight-indent-guides-character ?\┊
        highlight-indent-guides-delay 0.1))
  ;; (set-face-background 'highlight-indent-guides-odd-face "dimgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray"))

;; (use-package! copilot-chat
;;   :after copilot
;;   :config
;;   ;; Optional: set keybindings
;;   (define-key copilot-chat-mode-map (kbd "C-c C-c") #'copilot-chat-submit)
;;   (define-key copilot-chat-mode-map (kbd "C-c C-k") #'copilot-chat-clear-buffer)
;;   (define-key prog-mode-hook (kbd "C-c h") #'copilot-chat-display)) ;; Example keybinding to open chat

;; (use-package! move-text)

;; (use-package! drag-stuff
;;   :defer t
;;   :init
;;   (map! "M-<up>"    #'drag-stuff-up
;;         "M-<down>"  #'drag-stuff-down
;;         "M-<left>"  #'drag-stuff-left
;;         "M-<right>" #'drag-stuff-right))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t)
  ;; Optional: customize which segments to display (project, file, symbols)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols))
  ;; Optional: enable icons (requires all-the-icons package)
  (setq lsp-headerline-breadcrumb-icons-enable t))

(after! centaur-tabs
  (centaur-tabs-change-fonts "Lato" 110)
  (setq centaur-tabs-height 48
        centaur-tabs-style "wave"
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"))

;; (after! treemacs
;;   (set-face-attribute 'treemacs-root-face
;;                       nil
;;                       :family "Lato"
;;                       :height 125)
;;   (set-face-attribute 'treemacs-file-face
;;                       nil
;;                       :family "Lato"
;;                       :height 110)
;;   (set-face-attribute 'treemacs-directory-face
;;                       nil
;;                       :family "Lato"
;;                       :height 110))

(after! treemacs
  (setq doom-themes-treemacs-enable-variable-pitch t)
  ;; Ensure ALL treemacs faces use variable-pitch font
  (dolist (face '(treemacs-root-face
                  treemacs-git-unmodified-face
                  treemacs-git-modified-face
                  treemacs-git-renamed-face
                  treemacs-git-ignored-face
                  treemacs-git-untracked-face
                  treemacs-git-added-face
                  treemacs-git-conflict-face
                  treemacs-directory-face
                  treemacs-directory-collapsed-face
                  treemacs-file-face
                  treemacs-tags-face
                  ;; treemacs-header-face
                  ;; treemacs-help-button-face
                  treemacs-on-failure-pulse-face))
    (set-face-attribute face nil :family "Lato" :height 110)))

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally)))

;; Open treemacs and dired buffers on startup
;; Added by Windsurf - this didn't work
;; (add-hook! 'doom-init-ui-hook
;;   (defun +open-default-buffers-h ()
;;     "Open treemacs and dired buffers on startup."
;;     (treemacs)
;;     (dired "~/")))
