;; install)
;; mkdir -p ~/.emacs.d && echo '(load-file "~/dotfiles/emacs/init.el")' > ~/.emacs.d/init.el

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(setq-default indent-tabs-mode nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(require 'ido)
(ido-mode t)

;; calendar
(setq calendar-week-start-day 1
      calendar-day-abbrev-array ["Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"]
      calendar-day-name-array ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
      calendar-month-name-array ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
      )
(setq system-time-locale "C")


;; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (expand-file-name "~/doc/org/gtd.org") "Tasks")
         "* TODO %?\n  %i\n  %a\n  %T")
        ("j" "Journal" entry (file+datetree (expand-file-name "~/doc/org/journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")
        ("m" "memo" entry (file (expand-file-name "~/doc/org/memo.org"))
         "* %?\n  %i\n  %a\n  %T")
        ))

(setq org-agenda-files
      '((expand-file-name "~/doc/org/gtd.org")
        (expand-file-name "~/doc/org/journal.org")
        ))

(setq org-src-fontify-natively t   ; highlighting code block
      org-hide-emphasis-markers t  ; hide markup character
      org-startup-indented t       ; auto indent tree
      org-return-follows-link t    ; easy follow link
      org-time-stamp-custom-formats '("<%y-%m-%d %a>" . "<%y-%m-%d %a %H:%M>")
      org-completion-use-ido t
      org-log-done t
      )
(setq-default org-display-custom-times t)

(setq org-tag-alist '(("proj" . ?p)
                      ("work" . ?w)
                      ("env" . ?e)
                      ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAIT(w)" "|" "DONE(d!)" "CANCEL(c!)")))

(global-set-key (kbd "ESC ESC a") 'org-agenda)
(global-set-key (kbd "ESC ESC c") 'org-capture)

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(dolist
    (package '(
               auto-complete
               ace-jump-mode
               helm
               helm-cmd-t
               simplenote2
               zenburn-theme
               guide-key
               org-ac
               )
             )
  (unless (package-installed-p package)
    (package-install package)))


(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; helm
(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t

      helm-ff-lynx-style-map nil
      helm-ff-transformer-show-only-basename nil
      helm-input-idle-delay 0.1
      helm-idle-delay 0.1
      )

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-p") 'helm-mini)

;; helm-cmd-t
(require 'helm-cmd-t)
(global-set-key (kbd "ESC ESC t") 'helm-cmd-t)
(global-set-key (kbd "ESC ESC w") (lambda ()
                                  (interactive)
                                  (cd (expand-file-name "~/doc"))
                                  (helm :sources
                                        (list
                                         (helm-cmd-t-get-create-source-dir
                                          (expand-file-name "~/doc")))
                                        )))

;; simplenote2
;; add line to local init.el: (setq simplenote2-email ".." simplenote2-password "..")
(require 'simplenote2)
(simplenote2-setup)

(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
            (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
            (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))

;; guide-key
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4" "C-x" "C-c" "ESC"
        (org-mode "C-c C-x")))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/idle-delay 0.8)
(setq guide-key/popup-window-position :bottom)
(setq guide-key/text-scale-amount -1)

(add-hook 'org-mode-hook (lambda()
                           ;(guide-key/add-local-guide-key-sequence "C-c")
                           (guide-key/add-local-highlight-command-regexp "org-")
                           ))

(guide-key-mode 1)


;; view setting
(load-theme 'zenburn t)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")
