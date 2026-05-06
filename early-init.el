;; -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp-package); -*-

(setq package-enable-at-startup nil)

(setq initial-frame-alist
      '((foreground-color . "#ccd6f4")
        (background-color . "#1e1e2e")
        (fullscreen . "maximized")
        (font . "Iosevka Term SS08")
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)))

(setq default-frame-alist initial-frame-alist)
