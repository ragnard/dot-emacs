(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

(defconst emacs-start-time (current-time))

(defun report-time-since-load (&optional suffix)
  (message "Loading init...done (%.3fs)%s"
           (float-time (time-subtract (current-time) emacs-start-time))
           suffix))

(add-hook 'after-init-hook
          #'(lambda () (report-time-since-load " [after-init]"))
          t)

(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path
        (append (list (emacs-path "use-package"))
                (delete-dups load-path)
                (list (emacs-path "lisp")))))

(require 'use-package)

(setq use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics nil
      debug-on-error init-file-debug)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-hook 'after-init-hook #'garbage-collect t)

(defun reload-init ()
  (interactive)
  (org-babel-load-file "~/.emacs.d/init.org"))

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

(use-package dired
  :config
  (setq dired-dwim-target t)
  ;;(setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-ahl --group-directories-first"))

(use-package dired-x)
(use-package wdired)

(use-package wgrep
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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

(use-package htmlize
  :ensure t)

(use-package magit
  :ensure t
  :init
  (setq vc-handled-backends nil)
  :bind ("C-x m" . magit-status))

(use-package magit-svn
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package man
  :config
  (setq Man-notify-method 'pushy))

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

(use-package paredit
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.svelte\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (unbind-key (kbd "C-c C-l") web-mode-map))

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

(use-package comint
  :config
  (defun comint-clear-buffer ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (bind-keys :map comint-mode-map
             ("C-c M-o" . comint-clear-buffer)))

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

(use-package flycheck-rust
  :ensure t
  :init
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :bind
  (("C-c h" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package js
  :custom
  (js-indent-level 2))

(use-package yaml-mode
  :ensure t)

(use-package toml-mode
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

(use-package zig-mode
  :ensure t)

(use-package octave
  :ensure t
  :mode (("\\.m\\'" . octave-mode))
  :bind (:map octave-mode-map
              ("C-x C-e" . octave-send-line)
              ("C-c C-k" . octave-send-buffer)))

(use-package lua-mode
  :ensure t
  :custom
  (lua-default-application "lua"))

(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  :hook
  (before-save . gofmt-before-save))

;; A saner ediff
(use-package ediff
  :init
  (setq ediff-diff-options "-w")
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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
  :init
  (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package ripgrep
  :ensure t)

(use-package expand-region
  :ensure t
  :bind
  (("C-c '" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c C-d" . mc/mark-next-like-this)
   ("C-c C-a" . mc/mark-all-like-this)))
