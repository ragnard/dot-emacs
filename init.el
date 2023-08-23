;;; init.el -- my init
;;; Commentary:

;;; Code:

(defconst emacs-start-time (current-time))

(setq gc-cons-threshold 100000000)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set up load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(setq auto-save-dir (concat user-emacs-directory "auto-save/"))
(if (not (file-exists-p auto-save-dir))
    (make-directory auto-save-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name auto-save-dir) t )))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

(when is-mac
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; credit: yorickvP on Github
(defvar wl-copy-process nil)
(defun wl-copy (text)
  "Copy TEXT."
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  "Paste copied text."
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)

(defun bell ()
  "Rings the bell."
  (message "*ding*")
  (sit-for 0.2))

(setq ring-bell-function 'bell)


;; Highlight current line
(global-hl-line-mode 0)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sane defaults
;;(use-package smooth-scrolling)


;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
;; (global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Enable some stuff
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)


;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)



;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique


;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

;; (load-theme 'plain)

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(set-face-attribute
 'default nil
 :family "Iosevka SS08"
 :height 150
 :width 'normal
 :weight 'normal)

(require 'package)

(setq package-user-dir (locate-user-emacs-file "packages"))

;; Add melpa to package repos
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (setq package-pinned-packages
;;       '((cider              . "melpa-stable")
;;         ;(clojure-mode       . "melpa-stable")
;;         ))


(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :ensure t)

(use-package winner
  :init
  (winner-mode 1))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Save minibuffer history
(use-package savehist
  :init
  (savehist-mode 1)
  (setq history-length 1000))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

;; A saner ediff
(use-package ediff
  :init
  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; (use-package flx
;;   :ensure t)

;; (use-package ivy
;;   :ensure t
;;   :demand t
;;   :diminish ivy-mode
;;   :bind
;;   (:map ivy-mode-map
;;         ("C-'" . ivy-avy))
;;   :config
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; number of result lines to display
;;   (setq ivy-height 10)
;;   (setq ivy-re-builders-alist
;;         '((t . ivy--regex-plus)))

  ;; '((swiper . ivy--regex-plus)
  ;;   (t      . ivy--regex-fuzzy))

  ;; no regexp by default
  ;;(setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  ;; (setq ivy-re-builders-alist
  ;;       ;; allow input not in order
  ;;       '((t   . ivy--regex-ignore-order)))
  ;;)

;; (use-package counsel
;;   :ensure t
;;   :bind
;;   (("C-x C-i" . counsel-imenu)))

;; (use-package swiper
;;   :ensure t
;;   :bind
;;   (("C-s" . swiper-isearch)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package consult
  :ensure t
  :custom
  (consult-line-start-from-top t)
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package ripgrep
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package expand-region
  :ensure t
  :bind
  (("C-c '" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c C-d" . mc/mark-next-like-this)
   ("C-c C-a" . mc/mark-all-like-this)))

;; (use-package dired
;;   :init
;;   (setq dired-listing-switches "-aBhl --group-directories-first"))

(use-package dired
  :config
  (setq dired-dwim-target t)
  ;;(setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-ahl --group-directories-first"))

(use-package dired-x)
(use-package wdired)

(use-package wgrep
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :bind
  (("C-c C-e" . flycheck-list-errors)))

(use-package nhexl-mode
  :ensure t)

(use-package poke-mode
  :ensure t)

(use-package poke
  :ensure t)

(use-package org
  :ensure t
  :requires htmlize
  :config
  ;; (unbind-key "M-e" org-mode-map)
  (setq org-directory "~/notes"))

;; (use-package ob
;;   :ensure t
;;   :config
;;   (setq org-confirm-babel-evaluate nil)
;;   ;;(setq org-export-babel-evaluate nil)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (dot . t)
;;      (clojure . t))))

(use-package htmlize
  :ensure t)

(use-package comint
  :config
  (defun comint-clear-buffer ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (bind-keys :map comint-mode-map
             ("C-c M-o" . comint-clear-buffer)))

(use-package magit
  :ensure t
  :init
  (setq vc-handled-backends nil)
  :bind ("C-x m" . magit-status))

(use-package csv-mode
  :ensure t)

(use-package man
  :config
  (setq Man-notify-method 'pushy))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package company
  :ensure t
  :diminish company-mode
  :commands (company company-indent-or-complete-common)
  :config
  (global-company-mode)
  (setq company-idle-delay nil)
  :bind
  (:map company-mode-map
        ("TAB" . company-indent-or-complete-common)))

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  ((python-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (java-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (go-mode . eglot-ensure))
  :bind
  (("M-RET" . eglot-code-actions)
   ("C-c C-l" . eglot-format-buffer)))

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :bind-keymap
;;   ("C-c l" . lsp-command-map)
;;   :hook
;;   ((c-mode . lsp)
;;    (c++-mode . lsp)
;;    (go-mode . lsp)))

;; (use-package lsp-ui
;;   :ensure t)

(use-package paredit
  :ensure t)

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
  (setq clojure-indent-style :align-arguments)
  :config
  (put-clojure-indent 'match 'defun)
  (put-clojure-indent 'defrecord 'defun)
  (put-clojure-indent 'alt!! 'defun)
  (put-clojure-indent 'alt! 'defun)
  (put-clojure-indent 'fnk 'defun)
  (put-clojure-indent 'context* 'defun)
  (put-clojure-indent 'GET* 'defun)
  (put-clojure-indent 'POST* 'defun)
  (put-clojure-indent 'PUT* 'defun)
  (put-clojure-indent 'DELETE* 'defun))

(defun inf-clojure-load-complete ()
  "Load clojure completion support."
  (interactive)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat user-emacs-directory "clojure/complete.clj")))
    (inf-clojure-eval-buffer)))

;; (use-package inf-clojure
;;   :ensure t
;;   )

(use-package web-mode
  :ensure t
  :mode (("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (unbind-key (kbd "C-c C-l") web-mode-map))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :bind
  (("C-c h" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))
  ;(setq eldoc-idle-delay 0.1)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-popup-stacktraces t)
  (setq cider-repl-popup-stacktraces t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-use-overlays nil)
  (setq cider-repl-display-in-current-window nil)
  (setq cider-repl-prompt-function #'cider-repl-prompt-abbreviated)
  (setq cider-repl-display-help-banner nil)
  ;(setq cider-repl-tab-command #'company-indent-or-complete-common)
  )

;;   :config
;;   (defun cider-reset-system ()
;;     (interactive)
;;     (message "Resetting")
;;     (if (not (cider-connected-p))
;;         (message "Not connected to repl")
;;       (cider-nrepl-request:eval "(reset)" (cider-interactive-eval-handler (current-buffer)) "user")))
;;   (bind-keys :map cider-mode-map
;;              ("C-c C-r" . cider-reset-system))
;;   (bind-keys :map cider-repl-mode-map
;;              ("C-c C-r" . cider-reset-system)))

;; (defun cider--local-listening-tcp-ports ()
;;   (mapcar (lambda (s)
;;             (destructuring-bind (pid addr) (read s)
;;               (list pid (cadr (split-string addr ":")))))
;;           (split-string
;;            (shell-command-to-string "lsof -nP -iTCP@0.0.0.0 -sTCP:LISTEN | tail -n +2 | awk '{ printf \"(\\\"\%s\\\" \\\"\%s\\\")\\n\", $2, $9; }'")
;;            "\n" t)))

;; (defun cider--local-jvms ()
;;   (mapcar (lambda (s)
;;             (let ((parts (split-string s " ")))
;;               (list (car parts) (string-join (cdr parts) " "))))
;;           (split-string
;;            (shell-command-to-string "jps -lm | sed /sun.tools.jps.Jps/d")
;;            "\n" t)))

;; (defun cider--local-jvm-endpoints ()
;;   (let* ((local-ports (cider--local-listening-tcp-ports))
;;          (local-jvms (cider--local-jvms))
;;          (local-endpoints (mapcar (lambda (v)
;;                                    (destructuring-bind (pid port) v
;;                                      (list port pid (cadr (assoc pid local-jvms)))))
;;                                  local-ports)))
;;     (remove-if-not (lambda (v) (car (last v))) local-endpoints)))


;; (defun cider-select-local-endpoint ()
;;   (let* ((endpoints (mapcar (lambda (v)
;;                              (destructuring-bind (port pid main) v
;;                                (list (format "%s: %s @ %s" pid main port) port)))
;;                            (cider--local-jvm-endpoints)))
;;          (selection (completing-read "Endpoint: " endpoints)))
;;     (cdr (assoc selection endpoints))))

;; (defun cider-local (port)
;;   (interactive (cider-select-local-endpoint))
;;   (cider-connect "0.0.0.0" port))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook emacs-lisp-mode-hook)


(use-package rust-mode
  :ensure t
  :hook (rust-mode . eglot-ensure)
  :config
  ;(rust-enable-format-on-save)
  (flymake-mode-off)
  :bind
  (("C-c C-k" . rust-compile)
   ("C-c C-r" . rust-run)
   ("C-c C-t" . rust-test)))

(use-package toml-mode
  :ensure t)


(use-package flycheck-rust
  :ensure t
  :init
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

;; (use-package javascript-mode
;;   :init
;;   (setq js-indent-level 4))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package lua-mode
  :ensure t
  :config
  (setq lua-default-application "lua"))

(use-package protobuf-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package sparql-mode
  :ensure t)

(use-package ttl-mode
  :ensure t)

(use-package sql
  :ensure t)

(use-package sql-clickhouse
  :ensure t
  :config
  ;(setq sql-clickhouse-options '("-f" "PrettySpaceNoEscapes"))
  (setq sql-clickhouse-options nil))

(use-package yaml-mode
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (defun python-format-before-save-hook ()
    (elpy-format-code))

  (add-hook 'elpy-mode-hook
            (lambda () (add-hook 'before-save-hook
                                 #'python-format-before-save-hook nil
                                 'local)))
  :config
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv))
  :bind (:map python-mode-map
              ("C-x C-e" . elpy-shell-send-statement-and-step)
              ("C-c C-k" . elpy-shell-send-buffer)
              ("C-c C-z" . elpy-shell-switch-to-shell)))

(use-package octave
  :ensure t
  :mode (("\\.m\\'" . octave-mode))
  :bind (:map octave-mode-map
              ("C-x C-e" . octave-send-line)
              ("C-c C-k" . octave-send-buffer)))

(use-package hcl-mode
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

;; Functions (load all files in defuns-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Single char cursor movement
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)

;; Move by word
(global-set-key (kbd "M-u") 'backward-word)
;(global-set-key (kbd "M-U") 'backward-sexp)
(global-set-key (kbd "M-o") 'forward-word) ; was (prefix)
;(global-set-key (kbd "M-O") 'forward-sexp) ; was (prefix)

;; Move by paragraph
;;(global-set-key (kbd "M-S-U") 'scroll-down)
;;(global-set-key (kbd "M-S-O") 'scroll-up)


;; (global-set-key (kbd "M-i") 'back-to-indentation)
(global-set-key (kbd "M-I") 'backward-paragraph)
(global-set-key (kbd "M-K") 'forward-paragraph)

(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-D") 'delete-indentation)

(defun maybe-toggle-multiple-cursors (&optional arg)
   (interactive "P")
   (if (or (not mark-active) arg)
       (set-mark-command arg)
     (mc/edit-lines)))

(global-set-key (kbd "C-c SPC") 'maybe-toggle-multiple-cursors)

(global-set-key (kbd "C-c C-l") 'indent-buffer)

;; Make shell more convenient, and suspend-frame less
(global-set-key (kbd "C-z") 'shell)
(global-set-key (kbd "C-x M-z") 'suspend-frame)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

(defun pretty-format-xml ()
  "Pretty format as XML."
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "xmllint --format - "
                           (current-buffer)
                           t))

(defun pretty-format-json ()
  "Pretty format as JSON."
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           "jq ."
                           (current-buffer)
                           t))

(defun sudo-edit (&optional file-name)
  "Edit buffer or FILE-NAME as root."
  (interactive "P")
  (if (or file-name (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(require 'server)

(unless (server-running-p)
  (server-start))

;;(set-register ?i `(file . ,(buffer-file-name)))
;;(set-register ?n '(file . "/sudo:localhost:/etc/nixos/configuration.nix"))

;; timing

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed (float-time (time-subtract (current-time)
                                                       emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed)))
          t)

(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)
;;; init.el ends here
