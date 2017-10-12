;; Packages
(require 'package)

(defvar user-package-list '(
                     flycheck
                     flycheck-elixir
                     eslint-fix
                     
                     helm
                     column-marker
                     neotree
                     xclip
;                     drag-stuff
                     
                     clojure-mode
                     scala-mode
                     go-mode
                     markdown-mode
                     yaml-mode
                     lua-mode
                     dockerfile-mode
                     powershell
                     nginx-mode
                     elixir-mode
                     alchemist
                     js2-mode
                     
                     auto-complete
                     go-autocomplete

                     base16-theme
))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package user-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path "~/.emacs.d/custom/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(frame-background-mode (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Backward delete word
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

; See .Xresources: URxvt.keysym.C-BackSpace: \033[33~
(global-set-key (kbd "S-<f9>") 'backward-delete-word)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Tags
;(custom-set-variables
;  '(ac-etags-requires 2))
;(eval-after-load "etags"
;  '(progn (ac-etags-setup)))
;'(add-hook 'c-mode-common-hook 'ac-etags-ac-setup)
;(add-hook 'python-mode-hook 'ac-etags-ac-setup)

;; Semantic Mode
(require 'semantic/ia)
(semantic-mode 1)
(global-semantic-show-unmatched-syntax-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;; Neo Tree
(require 'neotree)
(setq neo-window-width 30)
(global-set-key (kbd "C-q") 'neotree-toggle)

;; Misc line settings
(defun set-linum-format ()
  (progn
     (defface linum-leading-zero
       `((t :inherit 'linum
            :foreground ,(face-attribute 'linum :background nil t)))
       "Face for displaying leading zeroes for line numbers in display margin."
       :group 'linum)

     (defun linum-format-func (line)
       (let ((w (length
                 (number-to-string (count-lines (point-min) (point-max))))))
         (concat
          (propertize (make-string (- w (length (number-to-string line))) ?0)
                      'face 'linum-leading-zero)
          (propertize (concat (number-to-string line) " ") 'face 'linum))))

     (setq linum-format 'linum-format-func)))

(global-linum-mode 1)
(global-visual-line-mode 1) ; wrap long lines

;; line highlighting
(global-hl-line-mode 1)

;; Turn Menu Bar off
(menu-bar-mode -1)

;; Use xclip
(xclip-mode 1)

;; drag-stuff
;(drag-stuff-global-mode 1)

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

(defun how-many-region (begin end regexp &optional interactive)
    "Print number of non-trivial matches for REGEXP in region.                    
Non-interactive arguments are Begin End Regexp"
    (interactive "r\nsHow many matches for (regexp): \np")
    (let ((count 0) opoint)
      (save-excursion
        (setq end (or end (point-max)))
        (goto-char (or begin (point)))
        (while (and (< (setq opoint (point)) end)
                    (re-search-forward regexp end t))
          (if (= opoint (point))
              (forward-char 1)
            (setq count (1+ count))))
        (if interactive (message "%d occurrences" count))
        count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq-default
  indent-tabs-mode nil
  tab-width 2
  )

(add-hook 'prog-mode-hook 'infer-indentation-style)

(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'go-mode-hook '(lambda () (setq tab-width 2)))

;; Color theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized/" t)
;;(load-theme 'solarized t)
(load-theme 'base16-tomorrow-night t)
(set-linum-format)
    
;; Rainbow Delimiters
; Solarized uses the same colour for some delimiters as the background colour... :(
;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;(require 'rainbow-delimiters)

;; Flycheck
(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-p") 'previous-error)
(add-hook 'python-mode-hook 'flycheck-mode)
;(add-hook 'prog-mode-hook #'flycheck-mode)
;(add-hook 'java-mode-hook 'flycheck-mode)

;; Javascript
(rassq-delete-all 'javascript-mode auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))


;; Helm
; Mostly taken from https://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
; Changed to "C-c h". Note: We must set "C-c h" globally, because we
; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-o") 'helm-semantic-or-imenu)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq
 helm-split-window-in-side-p       t ; open helm buffer inside current window, not occupy whole other window
 helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-search-library-in-sexp    t ; search for library in `require' and `declare-function' sexp.
 helm-M-x-fuzzy-match              t
 helm-buffers-fuzzy-matching       t
 helm-recentf-fuzzy-match          t
 helm-semantic-fuzzy-match         t
 helm-imenu-fuzzy-match            t
)

;;; helm-lean
; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
(set-face-attribute 'helm-source-header nil :height 0.1)
(setq
 helm-autoresize-max-height 30
 helm-autoresize-min-height 30
 helm-display-header-line nil
)
(helm-autoresize-mode 1)
(defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
(defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
(defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))

(helm-mode 1)

;; Scala
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

;; Java
(defun java-arglist-intro ()
      (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'java-arglist-intro)

(add-hook 'java-mode-hook
          (lambda()
            (local-unset-key (kbd "C-d"))))

;; Golang
(add-hook 'before-save-hook #'gofmt-before-save)

;; Misc keybindings
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "M-[ C") 'forward-word)
(global-set-key (kbd "M-[ D") 'backward-word)
(global-set-key (kbd "C-l") 'goto-line) ; eclipse muscle memory
(global-set-key (kbd "C-c C-c") 'recenter) ; rebind from C-l
(global-unset-key (kbd "C-z")) ; stop the fat-finger C-z suspending
(global-unset-key (kbd "C-v")) ; lame scrolling
(global-set-key (kbd "C-z") 'set-mark-command) ; alternative mark because cmder is lame 

;; Misc settings
(delete-selection-mode 1)
(setq
 vc-follow-symlinks t
 scroll-conservatively 0
 debug-on-error t
 create-lockfiles nil
 x-select-enable-clipboard t
 x-select-enable-primary t
 require-final-newline t
 vc-follow-symlinks t
)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(add-to-list 'auto-mode-alist '("/nginx/sites-\\(available\\|enabled\\)/" . nginx-mode))

(provide 'init)
;;; init.el ends here
