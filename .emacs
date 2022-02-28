(setq warning-suppress-log-types '((package reinitialization)))
(require 'package)
(package-initialize)
;; 加载新的库， M-x package-refresh-contents
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/"))
)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pyim magit monokai-theme evil neotree undo-tree lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq command-line-default-directory "D:/home/Asteruser/code")

(set-terminal-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-face-attribute 'default nil :family "Consolas Nerd Font Mono" :height 120)
;; Setting Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
            charset
            (font-spec :family "Microsoft Yahei" :height 120)))
(load-theme 'monokai t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default c-basic-offset 4)
(global-set-key (kbd "C-<tab>") 'other-frame)
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))
(setq visible-bell 0)

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(defun my-shell-mode-setup-function () 
  (when (fboundp 'company-mode))
    (company-mode -1))
(add-hook 'shell-mode-hook 'my-shell-mode-setup-function)
(add-hook 'eshell-mode-hook 'my-shell-mode-setup-function)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(server-start)

(add-to-list 'load-path "~/.emacs.d/elpa/evil-20220107.1733")
(require 'evil)
(evil-mode 1)
;;(global-evil-tabs-mode t)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

(add-to-list 'load-path "~/.emacs.d/elpa/neotree-20200324.1946")
(require 'neotree)
(global-set-key [f5] 'neotree-toggle)
(global-set-key [f8] 'neotree-refresh)

(setq default-input-method "pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)
