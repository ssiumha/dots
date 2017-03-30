;; install)
;; mkdir -p ~/.emacs.d && echo '(load-file "~/dotfiles/emacs/init.el")' > ~/.emacs.d/init.el

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'ido)
(ido-mode t)

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
               adoc-mode
               guide-key
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

;; adoc-mode
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))


;; guide-key
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4" "C-x" "C-c" "ESC"
        (org-mode "C-c C-x")))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/idle-delay 0.3)
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
