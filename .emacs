;; init.el -- Emacs configuration file

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'packages-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

;; Packages needed
(setq package-list
      '(monokai-theme))

;; Activate all the packages
(package-initialize)

;; Fetch the list of available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing package
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Auto Pairs 
(electric-pair-mode t)
;; Line Numbers
(global-linum-mode t)

(setq inhibit-startup-screen t)
(load-theme 'monokai t)
