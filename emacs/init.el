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
		   ))
  (unless (package-installed-p package)
    (package-install package)))


(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c p") 'helm-mini)
