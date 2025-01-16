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

(add-to-list 'exec-path "/opt/homebrew/bin")

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

;; Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
; (package-refresh-contents)

; (load-theme 'wombat t)
(use-package spacemacs-theme
             :ensure t
             :config
             (load-theme 'spacemacs-dark t))

(use-package org
             :defer t
             ; :hook
             ; (org-mode . (lambda () (set-face-attribute 'org-table nil :family "D2Coding" :height 180)))

             :config
             (setq org-cycle-max-level 2
                   org-startup-indented t ; +STARTUP: indent
                   org-confirm-babel-evaluate nil ; Evaluate code block without confirmation
                   org-log-done t ; or 'note
                   ; org-persist-directory "~/.local/emacs/org-persist"
                   org-persist-autoload t
                   org-agenda-format-date "%Y-%m-%d %a")

             (org-babel-do-load-languages 'org-babel-load-languages
                                          '((ruby . t)
                                            (emacs-lisp . t)
                                            (shell . t)
                                            (dot . t)))

             (org-link-set-parameters "open"
                                      :follow (lambda (path)
                                                      (start-process "open" nil "open" (expand-file-name path))))
             (org-link-set-parameters "chrome"
                                      :follow (lambda (link)
                                                      (start-process "open-chrome" nil "open" "-a" "Google Chrome.app" link)))
             (org-link-set-parameters "send-tmux"
                                      :follow (lambda (path)
                                                      (start-process "send-tmux" nil "tmux" "send-keys" path input "Enter")))
             )

(use-package org-sticky-header
             :ensure t
             :hook (org-mode . org-sticky-header-mode)
             :custom (org-sticky-header-full-path 'full))

(use-package org-sidebar
             :ensure t
             :commands
             (org-sidebar-tree org-sidebar-toggle)
             :custom
             (org-sidebar-tree-jump-to-header t)
             (org-sidebar-width 90)
             :bind
             ("C-c s" . org-sidebar-toggle))

; (use-package org-modern
;              :ensure t
;              :commands org-modern-visibility-mode
;              :hook (org-mode . org-modern-visibility-mode))
;; display-buffer-in-side-window를
;; olivetti, org-ql,

; TODO: remove when emacs v30
(use-package which-key
             :ensure t
             :init
             (setq which-key-idle-delay 0.2
                   which-key-popup-type 'minibuffer ; 'side-window|'frame
                   which-key-sort-order 'which-key-key-order-alpha)
             :config
             (which-key-mode))

(use-package company
             :ensure t
             :hook (org-mode . company-mode))
(use-package company-org-block
             :ensure t
             :after company
             :config
             (add-to-list 'company-backends 'company-org-block))
(use-package company-fuzzy
             :ensure t
             :hook (company-mode . company-fuzzy-mode)
             ; :init
             ; (setq company-fuzzy-sorting-backend 'alphabetic)
             :config
             (global-company-fuzzy-mode 1))

(use-package valign
             :ensure t
             :hook (org-mode . valign-mode))

(use-package fzf
             :ensure t
             :bind ("C-c f" . fzf)
             :config
             (setenv "FZF_DEFAULT_OPTS" ""
                     "FZF_DEFAULT_COMMAND" "")
             (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
                   fzf/executable "fzf"
                   fzf/git-grep-args "-i --line-number %s"
                   fzf/grep-command "rg --no-heading -nH"
                   ;; If nil, the fzf buffer will appear at the top of the window
                   fzf/position-bottom t
                   fzf/window-height 15)

             ; fzf-with-command (command action &optional directory as-filter initq)
             ; (fzf-with-command "git branch"
             ;                   "git checkout {}"
             ;                   "~/dots"
             ;                   t ; -> filter instead fzf filtering. fzf-grep-*..
             ;                   "") ; -> as-filter default --query
             ; fzf-with-entries (entries action &optional directory)
             ; (fzf-with-entries (list "a" "b" "c") 'print)
             )

(use-package evil
             :ensure t

             :init
             (evil-mode 1)
             (evil-set-undo-system 'undo-redo)

             :hook
             ; (add-hook 'org-read-date-minibuffer-setup-hook #'evil-local-mode)
             ; (add-hook 'calendar-mode-hook #'evil-local-mode)
             (minibuffer-setup . (lambda () (evil-insert-state) (evil-local-mode -1)))

             :config
             (setq evil-want-keybinding nil
                   evil-want-C-i-jump nil
                   ;evil-want-C-u-scroll t
                   evil-want-C-w-in-emacs-state t
                   evil-want-Y-yank-to-eol t)

             (evil-ex-define-cmd "reload-config" (lambda ()
                                                   (interactive)
                                                   (load-file (expand-file-name "~/.config/emacs/init.el"))
                                                   (message "Config reloaded!")))
             (evil-ex-define-cmd "tabe" 'tab-new)

             (evil-define-key 'normal org-mode-map
                              (kbd "Y") (lambda () (interactive) (evil-yank (point) (line-end-position)))
                              (kbd "<TAB>") #'org-cycle
                              (kbd "<S-TAB>") #'org-shifttab
                              (kbd "{") #'org-previous-visible-heading
                              (kbd "}") #'org-next-visible-heading
                              (kbd "(") #'org-backward-heading-same-level
                              (kbd ")") #'org-forward-heading-same-level
                              (kbd "C-w g f") #'org-open-at-point
                              (kbd "C-u") #'evil-scroll-up)

             (evil-define-key 'insert org-mode-map
                              (kbd "C-a") #'org-start-of-line
                              (kbd "C-e") #'org-end-of-line)

             (evil-define-key 'normal 'global
                              (kbd ",") 'evil-repeat-find-char
                              (kbd ";") 'evil-ex
                              (kbd "C-w t") 'my/org-open-link-or-new-tab)

             ; j -> org-agenda-goto-date
             ; k -> org-agenda-capture
             ; (with-eval-after-load 'org-sidebar
             ;   (evil-define-key 'normal org-sidebar-tree-map (kbd "k") 'previous-line)
             ;   (evil-define-key 'normal org-sidebar-tree-map (kbd "j") 'next-line))
             )

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

(dolist (file (directory-files "~/dots/emacs/lisp" t "\\.el$"))
  (load file))
