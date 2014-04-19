;; Smart-home
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

;; Tags
;;; functions
(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update-single(filename)  
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(add-hook 'after-save-hook 'gtags-update-hook)

(autoload 'gtags-mode "gtags" "" t)

(add-hook 'c++-mode-hook 
   '(lambda () 
      (gtags-mode t)
))

(add-hook 'python-mode-hook 
   '(lambda () 
      (gtags-mode t)
))

;;; bindings
(global-set-key (kbd "M-<") 'gtags-pop-stack)
(global-set-key (kbd "M->") 'gtags-find-tag-from-here)
(global-set-key (kbd "M-o") 'gtags-find-tag)

(semantic-mode 1)
(global-semantic-show-unmatched-syntax-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(require 'semantic/ia)

;; Misc line settings
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq line-move-visual nil)
(global-visual-line-mode 1)

;; line highlighting
(global-hl-line-mode 1)
(set-face-background 'hl-line "gray13")
(set-face-foreground 'highlight nil)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Marmalade
(require 'package)
(setq package-list '(auto-complete clojure-mode color-theme color-theme-solarized flycheck pylint python-mode))

; list the repositories containing them
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Turn Menu Bar off
(menu-bar-mode -1)

;; Autosave settings
(setq backup-directory-alist `(("." . "~/.autosave")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Ident whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Color theme
(load-theme 'solarized-dark t)

;; Flymake
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))

(add-hook 'c++-mode-hook 
   '(lambda () 
      (flymake-mode t)
))

(add-hook 'python-mode-hook 
   '(lambda () 
      (flymake-mode t)
))

(add-to-list 'flymake-allowed-file-name-masks
    '("\\.py\\'" flymake-pylint-init)))

(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

(global-set-key (kbd "C-p") 'my-flymake-show-next-error)

;; Misc keybindings
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-[ C") 'forward-word)
(global-set-key (kbd "M-[ d") 'backward-word)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;; Misc settings
(delete-selection-mode 1)
