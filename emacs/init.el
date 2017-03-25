;; install)
;; mkdir -p ~/.emacs.d && echo '(load-file "~/dotfiles/emacs/init.el")' > ~/.emacs.d/init.el

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)

(tool-bar-mode -1)

(require 'ido)
(ido-mode t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(dolist (package '(
		   auto-complete ace-jump-mode helm
		   simplenote2
		   ))
  (unless (package-installed-p package)
    (package-install package)))


(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Helm
(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t
)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c p") 'helm-mini)

;; simplenote2
(require 'simplenote2)
(simplenote2-setup)

(add-hook 'simplenote2-note-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-t") 'simplenote2-add-tag)
            (local-set-key (kbd "C-c C-c") 'simplenote2-push-buffer)
            (local-set-key (kbd "C-c C-d") 'simplenote2-pull-buffer)))