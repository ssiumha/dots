;; Basic UI
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(global-auto-revert-mode 1) ; apply file change immediately
(global-tab-line-mode -1) ; window level tab

(show-paren-mode 1)
(tab-bar-mode 1) ; frame level tab
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq tab-bar-show 1
      tab-bar-close-button-show nil
      show-paren-style 'expression
      shift-select-mode nil
      inhibit-startup-screen t
      custom-safe-themes t)

(setq-default line-spacing 3
              tab-width 2
              indent-tabs-mode nil)

(set-face-attribute 'default nil :height 180)

(add-to-list 'exec-path "/opt/homebrew/bin")

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
             :defer t
             :init
             (load-theme 'spacemacs-dark t))
(use-package rainbow-delimiters
             :ensure t
             :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-parentheses
             :ensure t
             :hook (prog-mode . highlight-parentheses-mode))
(use-package smartparens
             :ensure t)

(use-package recnetf
             :ensure nil
             :hook (after-init . recentf-mode)
             :custom
             (recentf-max-saved-items 20)
             (recentf-auto-cleanup 'never)
             :config
             (add-to-list 'recentf-exclude "\\.git/\\'"))

(use-package desktop
             :ensure nil
             :hook ((after-init . desktop-read)
                    (kill-emacs . desktop-save-in-desktop-dir))
             :config
             (desktop-save-mode 1)
             (setq desktop-base-file-name "emacs.desktop"
                   desktop-save 'if-exists
                   desktop-restore-frames t
                   desktop-auto-save-timeout 5
                   desktop-load-locked-desktop t)
             (if (eq system-type 'windows-nt)
                 (setq desktop-dirname (expand-file-name "~/.emacs.d"))
                 (setq desktop-dirname "~/.local/emacs/desktop"))
             )

(use-package org
             :ensure nil
             :defer t
             ; :hook
             ; (org-mode . (lambda () (set-face-attribute 'org-table nil :family "D2Coding" :height 180)))
             :config
             (require 'org-tempo)
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

             (org-link-set-parameters "explorer" :follow #'open-explorer)
             (org-link-set-parameters "chrome"   :follow #'open-chrome)

             (org-link-set-parameters "open"
                                      :follow (lambda (path)
                                                      (start-process "open" nil "open" (expand-file-name path))))
             (org-link-set-parameters "send-tmux"
                                      :follow (lambda (path)
                                                      (start-process "send-tmux" nil "tmux" "send-keys" path input "Enter")))
             )

(use-package org-bullets
             :ensure t
             :hook (org-mode . org-bullets-mode)
             :config
             (setq org-bullets-bullet-list '("◉" "○" "▶" "◆" "▶" "▷")))

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
(use-package company-capf
             :after company
             :config
             (add-to-list 'company-backends 'company-capf))

(use-package valign
             :ensure t
             :hook (org-mode . valign-mode))

; (use-package fzf
;              :ensure t
;              :bind ("C-c f" . fzf)
;              :config
;              (setenv "FZF_DEFAULT_OPTS" ""
;                      "FZF_DEFAULT_COMMAND" "")
;              (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
;                    fzf/executable "fzf"
;                    fzf/git-grep-args "-i --line-number %s"
;                    fzf/grep-command "rg --no-heading -nH"
;                    ;; If nil, the fzf buffer will appear at the top of the window
;                    fzf/position-bottom t
;                    fzf/window-height 15)
;              ; fzf-with-command (command action &optional directory as-filter initq)
;              ; (fzf-with-command "git branch"
;              ;                   "git checkout {}"
;              ;                   "~/dots"
;              ;                   t ; -> filter instead fzf filtering. fzf-grep-*..
;              ;                   "") ; -> as-filter default --query
;              ; fzf-with-entries (entries action &optional directory)
;              ; (fzf-with-entries (list "a" "b" "c") 'print)
;              )

(use-package evil-collection
             :after evil
             :ensure t
             :config
             (evil-collection-init))

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
                   evil-want-C-u-scroll t
                   evil-want-C-w-in-emacs-state t
                   evil-want-Y-yank-to-eol t
                   evil-want-integration t
                   evil-shift-width 2)

             (evil-define-key 'normal 'global
                              (kbd "g t") 'tab-bar-switch-to-next-tab
                              (kbd "g T") 'tab-bar-switch-to-prev-tab
                              (kbd "Y") (lambda () (interactive) (evil-yank (point) (line-end-position)))
                              (kbd ",") 'evil-repeat-find-char
                              (kbd ":") 'evil-repeat-find-char
                              (kbd ";") 'evil-ex
                              (kbd "C-u") 'evil-scroll-up
                              (kbd "C-S-h") 'sp-backward-sexp
                              (kbd "C-S-l") 'sp-forward-sexp
                              (kbd "C-S-k") 'sp-backward-up-sexp
                              (kbd "C-S-j") 'sp-down-sexp
                              (kbd "C-w t") 'my/org-open-link-or-new-tab
                              (kbd "SPC p p") 'counsel-file-jump-from-git-root
                              (kbd "SPC p [") 'counsel-recentf
                              )

             (evil-define-key 'insert 'global
                              (kbd "C-a") #'org-start-of-line
                              (kbd "C-e") #'org-end-of-line)

             (evil-define-key 'normal org-mode-map
                              (kbd "<TAB>") #'org-cycle
                              (kbd "<S-TAB>") #'org-shifttab
                              (kbd "{") #'org-previous-visible-heading
                              (kbd "}") #'org-next-visible-heading
                              (kbd "(") #'org-backward-heading-same-level
                              (kbd ")") #'org-forward-heading-same-level
                              (kbd "C-w g f") #'org-open-at-point)

             ; j -> org-agenda-goto-date
             ; k -> org-agenda-capture
             ; (with-eval-after-load 'org-sidebar
             ;   (evil-define-key 'normal org-sidebar-tree-map (kbd "k") 'previous-line)
             ;   (evil-define-key 'normal org-sidebar-tree-map (kbd "j") 'next-line))

             (defun eval-current-block-or-region ()
               (interactive)
               (if (use-region-p)
                   (eval-region (region-beginning) (region-end))
                 (eval-defun nil)))
             (evil-define-key 'normal emacs-lisp-mode-map
                              (kbd "C-c C-c") 'eval-current-block-or-region)

             (evil-ex-define-cmd "reload-config" (lambda ()
                                                   (interactive)
                                                   (load-file (expand-file-name "~/.config/emacs/init.el"))
                                                   (message "Config reloaded!")))

             (evil-define-command evil-tabe (filename)
               (interactive "<f>")
               (tab-new)
               (find-file filename))
             (evil-ex-define-cmd "tabe" 'evil-tabe)

             (evil-define-command evil-q ()
               (interactive)
               (if (and (bound-and-true-p tab-bar-mode)
                        (> (length (tab-bar-tabs)) 1))
                   (tab-close)
                 (kill-buffer)))
             (evil-ex-define-cmd "q" 'evil-q)
             )

; M-o ivy-dispatching-done
(use-package ivy
             :ensure t
             :init
             (ivy-mode 1)
             :config
             ;; (setq ivy-use-virtual-buffers t)
             (setq ivy-height 15
                   enable-recursive-minibuffers t
                   ivy-re-builders-alist '((t . ivy--regex-fuzzy))
                   ;; ivy-sort-matches-functions-alist '((t . nil))
                   ;; ivy-sort-matches-functions-alist '((t . ivy--prefix-sort))
                   )
             )

(use-package ivy-prescient
             :ensure t
             :config
             (ivy-prescient-mode 1))

(use-package counsel
             :ensure t
             :after ivy
             :config
             (counsel-mode 1)

             (defun counsel-file-jump-from-git-root ()
               "Run `counsel-file-jump` from the root of the current Git repository if available."
               (interactive)
               (let ((git-root (locate-dominating-file default-directory ".git")))
                 (counsel-file-jump "" (or git-root default-directory))))

             (dolist (command '(counsel-file-jump counsel-find-file counsel-recentf counsel-file-jump-from-git-root))
               (ivy-add-actions
                command
                '(("t" (lambda (file)
                         (tab-new)
                         (find-file (expand-file-name file)))
                   "Open in new tab"))))
             )

(use-package vimrc-mode
             :ensure t
             :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(use-package lua-mode
             :ensure t
             :mode ("\\.lua\\'" . lua-mode))

(use-package web-mode
             :ensure t
             :mode ("\\.tsx\\'" . web-mode)
             :config
             (setq-default indent-tabs-mode nil
                           tab-width 2)
             (setq web-mode-markup-indent-offset 2
                   web-mode-css-indent-offset 2
                   web-mode-code-indent-offset 2
                   web-mode-enable-auto-quoting nil
                   web-mode-content-types-alist '(("jsx" . "\\.tsx\\'"))
                   typescript-indent-level 2))

(use-package origami
             :ensure t
             :hook (prog-mode . origami-mode))

(use-package magit
             :ensure t
             :bind ("C-x g" . magit-status))

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

(defun open-chrome (link)
  (if (eq system-type 'darwin)
      (start-process "open-chrome" nil "open" "-a" "Google Chrome.app" link)
    (start-process "open-chrome" nil "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" link)))

(defun open-explorer (path)
  (if (eq system-type 'windows-nt)
      (start-process "open-explorer" nil "explorer" path)
    (start-process "open-explorer" nil "open" path)))

(defun restore-focus ()
  (if (eq system-type 'windows-nt)
      (start-process "restore-focus" nil "powershell"
                     "-Command" "(New-Object -ComObject WScript.Shell).AppActivate('Emacs')")
    (start-process "restore-focus" nil "osascript" "-e" "tell application \"Emacs\" to activate")))

(dolist (file (directory-files "~/dots/emacs/lisp" t "\\.el$"))
  (load file))

; (with-output-to-temp-buffer "*Log Buffer*"
;   (dolist (tab (tab-bar-tabs))
;     (princ (format "== %s\n" (car tab)))
;     (princ (format " %s\n" (string= "*Deno Server*" (alist-get 'name tab))))
;     (princ (format "\t%s\n" (alist-get 'buffer (alist-get 'ws tab))))
;     (princ "----\n")
;     (princ (format "\t%s\n" (alist-get 'name tab)))
;     (princ (format "\t%s\n" (cdr tab)))
;     )
;   )
