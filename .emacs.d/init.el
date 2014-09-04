;; Packages
(require 'package)
(setq package-list '(
                     auto-complete
                     clojure-mode
                     scala-mode
                     flycheck))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/custom/")

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Smart-home
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

;; ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(add-hook 'python-mode-hook
          (lambda ()
            (ggtags-mode 1)))

;; Semantic Mode
(semantic-mode 1)
(global-semantic-show-unmatched-syntax-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(require 'semantic/ia)

;; Misc line settings
(global-linum-mode 1)
(setq
 linum-format "%4d "
 line-move-visual nil)
(global-visual-line-mode 1) ; wrap long lines

;; line highlighting
(global-hl-line-mode 1)

;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Turn Menu Bar off
(menu-bar-mode -1)

;; Autosave settings
(setq backup-directory-alist `((".*" . "~/.autosave")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Indentation
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized/" t)
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (load-theme 'solarized-dark t))))
    (load-theme 'solarized-dark t))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-pylintrc "~/.pylintrc")

(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-p") 'previous-error)

;; Scala
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; Java
(defun java-arglist-intro ()
      (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'java-arglist-intro)

;; Misc keybindings
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-[ C") 'forward-word)
(global-set-key (kbd "M-[ D") 'backward-word)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
(global-set-key (kbd "C-k") 'recenter)
(global-unset-key (kbd "C-z")) ; stop the fat-finger C-z suspending

;; Misc settings
(delete-selection-mode 1)
(setq
 vc-follow-symlinks t
 scroll-conservatively 0
 debug-on-error t
 create-lockfiles nil
 x-select-enable-clipboard t
 x-select-enable-primary t
)
(show-paren-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
