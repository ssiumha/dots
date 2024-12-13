;; Basic UI
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq tab-bar-show 1)
(setq tab-bar-close-button-show nil)

(setq inhibit-startup-screen t)

(setq-default line-spacing 3)
(set-face-attribute 'default nil :height 180)
(load-theme 'wombat t)

;; Desktop Mode
(desktop-save-mode 1)
(setq desktop-save t)
(setq desktop-restore-frames t)
(setq desktop-auto-save-timeout 5)
(setq desktop-dirname "~/.local/emacs/desktop"
      desktop-base-file-name "emacs.desktop")

; (setq initial-frame-alist
;       '((top . 0)
;         (left . -10)
;         (width . 120)
;         (height . 40)
;         (display . ":1")))

;; Directory
(setq package-user-dir "~/.local/emacs/elpa")
(setq backup-directory-alist `(("." . "~/.local/emacs/tmp/"))
      backup-by-copying t)

;; Edit
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; Org
(setq org-startup-indented t) ; +STARTUP: indent
(setq org-confirm-babel-evaluate nil) ; Evaluate code block without confirmation
(org-babel-do-load-languages
  'org-babel-load-languages
  '((ruby . t)
    (emacs-lisp . t)
    (shell . t)))

;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
; (package-refresh-contents)

(use-package org-sticky-header
             :ensure t
             :hook (org-mode . org-sticky-header-mode)
             :custom (org-sticky-header-full-path 'full))

(use-package spacemacs-theme)
(load-theme 'spacemacs-dark)

;; Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)

(evil-mode 1)
(setq evil-want-keybinding nil)
(evil-set-undo-system 'undo-redo)

(evil-ex-define-cmd "reload-config" (lambda ()
                                      (interactive)
                                      (load-file (expand-file-name "~/.config/emacs/init.el"))
                                      (message "Config reloaded!")))
(evil-ex-define-cmd "tabe" 'tab-new)

; (org-element-context) -> (link (:type ifle :path ./test.org :format bracket :raw-link ./test.or ...))
; (org-element-property :type (org-element-context)) -> "file"
(defun my/org-open-link-or-new-tab ()
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-property :type context))
         (path (org-element-property :path context)))
    (if (and (string= type "file") path)
        (progn
          (tab-new)
          (find-file (expand-file-name path)))
        (org-open-at-point))))

;; Evil Keybindings
(with-eval-after-load 'evil
  (evil-global-set-key 'normal (kbd ",") 'evil-repeat-find-char)
  (evil-global-set-key 'normal (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd "C-w t") 'my/org-open-link-or-new-tab))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "<TAB>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "<S-TAB>") #'org-shifttab)
  (evil-define-key 'normal org-mode-map (kbd "{") #'org-previous-visible-heading)
  (evil-define-key 'normal org-mode-map (kbd "}") #'org-next-visible-heading)
  (evil-define-key 'normal org-mode-map (kbd "(") #'org-backward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd ")") #'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "C-w g f") #'org-open-at-point)
  (evil-define-key 'normal org-mode-map (kbd "C-u") #'evil-scroll-up)
  (evil-define-key 'insert org-mode-map (kbd "C-a") #'org-start-of-line)
  (evil-define-key 'insert org-mode-map (kbd "C-e") #'org-end-of-line))

; (add-hook 'org-read-date-minibuffer-setup-hook #'evil-local-mode)
; (add-hook 'calendar-mode-hook #'evil-local-mode)
(add-hook 'minibuffer-setup-hook '(lambda () (evil-insert-state) (evil-local-mode -1)))

;; Custom-set
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" default))
 '(package-selected-packages '(spacemacs-theme sis org-sticky-header markdown-mode evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
