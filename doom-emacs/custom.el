;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function 'split-window-horizontally)
 '(safe-local-variable-values
   '((eval progn
      "This is code run by LilyPond's .dir-locals.el to fix indentation according to our standards.  If Emacs is warning you that this 'may not be safe', just accept running it."
      (put 'match 'scheme-indent-function 1)
      (put 'match-lambda 'scheme-indent-function 0)
      (put 'match-lambda* 'scheme-indent-function 0)
      (put 'match-let 'scheme-indent-function 'scheme-let-indent)
      (put 'match-let* 'scheme-indent-function 1)
      (put 'match-letrec 'scheme-indent-function 1)
      (put 'and-let* 'scheme-indent-function 1)
      (put 'with-syntax 'scheme-indent-function 1)
      (put 'eval-when 'scheme-indent-function 1)
      (put 'syntax-rules 'scheme-indent-function 'defun)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
