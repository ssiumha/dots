;; Basic UI
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq tab-bar-show 1
      tab-bar-close-button-show nil)

(setq inhibit-startup-screen t
      custom-safe-themes t)

(setq-default line-spacing 3)

(set-face-attribute 'default nil :height 180)
(add-hook 'org-mode-hook (lambda ()
                           (set-face-attribute 'org-table nil :family "D2Coding" :height 180)))

; (load-theme 'wombat t)

;; Desktop Mode
(desktop-save-mode 1)
(setq desktop-save t
      desktop-restore-frames t
      desktop-auto-save-timeout 5
      ; desktop-dirname "~/.local/emacs/desktop"
      desktop-base-file-name "emacs.desktop")

; (setq initial-frame-alist
;       '((top . 0)
;         (left . -10)
;         (width . 120)
;         (height . 40)
;         (display . ":1")))

;; Directory
(setq ;package-user-dir "~/.local/emacs/elpa"
      backup-directory-alist `(("." . "~/.local/emacs/backup"))
      backup-by-copying t
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      delete-auto-save-files t)

;; Edit
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq evil-shift-width 2)

;; Org
(setq org-startup-indented t ; +STARTUP: indent
      org-confirm-babel-evaluate nil ; Evaluate code block without confirmation
      org-log-done t ; or 'note
      ; org-persist-directory "~/.local/emacs/org-persist"
      org-persist-autoload t)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((ruby . t)
    (emacs-lisp . t)
    (shell . t)
    (dot . t)))

(org-link-set-parameters
  "open"
  :follow (lambda (path)
            (start-process "open" nil "open" (expand-file-name path))))

(add-to-list 'exec-path "/opt/homebrew/bin")
(org-link-set-parameters
  "send-tmux"
  :follow (lambda (path)
            (start-process "send-tmux" nil "tmux" "send-keys" path input "Enter")))
;TODO: (let ((input (read-string "Input: ")))
;  (setq-local tmux-target-pane)
;  (message "Input: %s" tmux-target-pane))

;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
; (package-refresh-contents)

(use-package spacemacs-theme)
(load-theme 'spacemacs-dark)

(use-package org-sticky-header
             :ensure t
             :hook (org-mode . org-sticky-header-mode)
             :custom (org-sticky-header-full-path 'full))
; (use-package org-sidebar
;              :ensure t
;              :commands (org-sidebar-tree org-sidebar-toggle)
;              :bind ("C-c s" . org-sidebar-toggle))
; (use-package org-modern
;              :ensure t
;              :commands org-modern-visibility-mode
;              :hook (org-mode . org-modern-visibility-mode))
;; display-buffer-in-side-window를
;; olivetti, org-ql,

;; Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)

(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(setq evil-want-keybinding nil)

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

(dolist (file (directory-files "~/dots/emacs/lisp" t "\\.el$"))
  (load file))
