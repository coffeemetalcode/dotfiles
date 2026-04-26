;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setenv "NPM_PACKAGES" "$HOME/.nvm/versions/node/v22.16.0/bin")
(add-to-list 'exec-path "$HOME/.nvm/versions/node/v22.16.0/bin")

;; Optional: Manual command configuration if auto-detection fails
(setq lsp-clients-angular-language-server-command
      '("node" "~/.npm-packages/lib/node_modules/@angular/language-server"
        "--ngProbeLocations" "~/.npm-packages/lib/node_modules"
        "--tsProbeLocations" "~/.npm-packages/lib/node_modules"
        "--stdio"))

(setq confirm-kill-emacs #'yes-or-no-p) ;; Force confirmation prompt when exiting Emacs

(server-start) ;; Start the Emacs server for emacsclient support

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
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-tokyo-night)
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
;; etc).i
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-hook! prog-mode
  (setq tab-width 2 ;; default tab indent
        c-basic-offset 4 ;; C/C++/Java indent
        indent-tabs-mode nil)) ;; use spaces instead of tabs

(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2
        html-indent-offset 2
        sgml-basic-offset 2)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'indent-tabs-mode) nil))

(after! html-mode
  (setq sgml-basic-offset 2
        html-indent-offset 2)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'indent-tabs-mode) nil))

(after! css-mode
  (setq css-indent-offset 2))

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode t)

;; Make window dividers wider for easier mouse grabbing
(setq window-divider-default-places t
      window-divider-default-bottom-width 3
      window-divider-default-right-width 3)

(setq doom-font (font-spec
                 :family "Fira Code"
                 :size 15)
      doom-variable-pitch-font (font-spec
                                :family "Lato"
                                :size 15))

;; Automatically trigger Lilypond Mode for Lilypond files
(add-to-list 'load-path "~/.local/share/applications/lilypond-2.24.4/share/emacs/site-lisp")
(load "lilypond-init.el")
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))

;; Make LilyPond-mode inherit prog-mode behavior and fix comment indentation
(defun my/lilypond-indent-line ()
  "Indent current line, with smarter handling for comment lines."
  (if (save-excursion
        (beginning-of-line)
        (looking-at "^\\s-*%"))  ; Line is a comment
      ;; For comment lines: indent based on previous line + context
      (let* ((prev-indent
              (save-excursion
                (forward-line -1)
                (while (and (not (bobp))
                            (looking-at "^\\s-*$"))  ; Skip blank lines
                  (forward-line -1))
                (current-indentation)))
             (prev-line-opens
              (save-excursion
                (forward-line -1)
                (while (and (not (bobp))
                            (looking-at "^\\s-*$"))
                  (forward-line -1))
                (end-of-line)
                (skip-chars-backward " \t")
                (member (char-before) '(?{ ?< ?\())))
             (target-indent
              (if prev-line-opens
                  (+ prev-indent 2)  ; Indent one level after opening brace
                prev-indent)))
        (indent-line-to target-indent))
    ;; For non-comment lines: use LilyPond's default indentation
    (LilyPond-indent-line)))

(add-hook 'LilyPond-mode-hook
          (lambda ()
            (run-hooks 'prog-mode-hook)
            (setq-local indent-line-function #'my/lilypond-indent-line)
            ;; Format (indent) buffer on save
            (add-hook 'before-save-hook
                      (lambda ()
                        (indent-region (point-min) (point-max)))
                      nil t)))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-c C-k" . 'copilot-clear-overlay)
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(lisp-interaction-mode 2))
  (add-to-list 'copilot-indentation-alist '(common-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(lisp-mode)))

(use-package! copilot-chat
  :after copilot
  (request org markdown-mode))

(use-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-delay 0.1))

(use-package! drag-stuff
  :defer t
  :init
  (map! "M-<up>"    #'drag-stuff-up
        "M-<down>"  #'drag-stuff-down))

(use-package! lsp-headerline
  :after lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(project file symbols)
        lsp-headerline-breadcrumb-icons-enable t))

(after! eglot
  (add-to-list 'eglot-stay-out-of 'flymake))

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

(after! treemacs
  (treemacs-follow-mode 1)
  ;; Prevent treemacs from scrolling on file save
  (setq treemacs-silent-refresh t
        treemacs-silent-filewatch t)
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
                  treemacs-on-failure-pulse-face))
    (set-face-attribute face nil :family "Lato" :height 110)))

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally)))

;; (after! vterm
;;   (set-popup-rule! "^\\*doom:vterm-popup" :side 'right :size 0.3 :select t :quit nil :ttl 0))

(after! doom-modeline
  (setq doom-modeline-check 'full
        doom-modeline-check-icon t))

(after! flycheck
  (add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck)
  (add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck))

(setq demap-minimap-window-width 15)
