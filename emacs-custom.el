(require 'package)
;(require 'whitespace)

(add-to-list 'load-path "~/.emacs.d/no-elpa")

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Activate the packages
(package-initialize)

;; List of packages
(setq package-list 
      '(zenburn-theme flycheck autopair sphinx-doc neotree slime request auto-complete ac-etags))

;; List available packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Start as a server
;; (server-start)

;; Dismiss start-up screen
(setq inhibit-startup-screen t)

(global-set-key [f8] 'neotree-toggle)

;; Reload file when file changed on disk (change of branch for instance)
(global-auto-revert-mode t)

;; Load theme
(load-theme 'zenburn t)

;; Maximise frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Activate Flycheck instead of Flymake
(global-flycheck-mode)

;; Line number on the side
(global-linum-mode t)

;; Highlight lines > 80 chars
;; (setq whitespace-line-column 80)
;(setq whitespace-style '(face lines-tail))
;(add-hook 'prog-mode-hook 'whitespace-mode)

;; Python mode
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))

;; Activate autopair
(autopair-global-mode)
;; Mathcing Delimiters
(show-paren-mode)

;; Turn off the alarm bell completely
(setq ring-bell-function 'ignore)

;; Copy paste outside emacs
(setq x-select-enable-clipboard t)

;; SBCL as default Lisp
(setq inferior-lisp-program "/homes/guiba/sbcl/bin/sbcl")

;; Activate interactively do things mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere  t)

;; Disable toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Default auto-complete config
(ac-config-default)
;; Distinguish case
(setq ac-ignore-case nil)
;; ac-etags
(eval-after-load "etags"
  '(progn
     (ac-etags-setup)))

(add-hook 'python-mode 'ac-etags-ac-setup)
(add-hook 'javascript-mode 'ac-etags-setup)

;; Font settings
(set-face-attribute 'default nil
		    :font "Monospace 10"
		    ;:family  "Monospace 10";"charter"
		    :height 115
		    :weight  'normal
		    :width 'normal)

;; comment // uncomment region key binding
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-etags auto-complete zenburn-theme spacemacs-theme yaml-mode sphinx-doc slime request qml-mode nov neotree json-mode gotham-theme flycheck dracula-theme autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

