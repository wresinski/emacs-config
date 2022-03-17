;; 初始化设置
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

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-set-key (kbd "C-q") 'other-frame)

;;(setq visible-bell 0)
(setq ring-bell-function 'ignore)

(xterm-mouse-mode 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq compilation-scroll-output t)

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun my-shell-mode-setup-function () 
  (when (fboundp 'company-mode))
    (company-mode -1))
(add-hook 'shell-mode-hook 'my-shell-mode-setup-function)
(add-hook 'eshell-mode-hook 'my-shell-mode-setup-function)


(setq-default c-basic-offset 4)
;;linux kernel style
;;(defun c-lineup-arglist-tabs-only (ignored)
;;  "Line up argument lists by tabs, not spaces"
;;  (let* ((anchor (c-langelem-pos c-syntactic-element))
;;         (column (c-langelem-2nd-pos c-syntactic-element))
;;         (offset (- (1+ column) anchor))
;;         (steps (floor offset c-basic-offset)))
;;    (* (max steps 1)
;;       c-basic-offset)))
;;
;;(add-hook 'c-mode-common-hook
;;          (lambda ()
;;            ;; Add kernel style
;;            (c-add-style
;;             "linux-tabs-only"
;;             '("linux" (c-offsets-alist
;;                        (arglist-cont-nonempty
;;                         c-lineup-gcc-asm-reg
;;                         c-lineup-arglist-tabs-only))))))
;;
;;(add-hook 'c-mode-hook
;;          (lambda ()
;;            (let ((filename (buffer-file-name)))
;;              ;; Enable kernel mode for the appropriate files
;;              (when (and filename
;;                         (string-match (expand-file-name "~/src/linux-trees")
;;                                       filename))
;;                (setq indent-tabs-mode t)
;;                (setq show-trailing-whitespace t)
;;                (c-set-style "linux-tabs-only")))))

(server-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 包管理器设置
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(setq package-check-signature nil) ;个别时候会出现签名校验失败

(require 'package)

;; 初始化包管理器
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; 刷新软件源索引
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t) ;不用每个包都手动添加:ensure t关键字
  (setq use-package-always-defer t) ;默认都是延迟加载，不用每个包都手动添加:defer t
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 加载插件
(require 'use-package)

(use-package zenburn-theme
  :init
  (setq zenburn-override-colors-alist
      '(("zenburn-bg" . "#2B2B2B")
	("zenburn-bg-1" . "#3F3F3F")))
  (load-theme 'zenburn t))

(use-package evil
  :init
  (evil-mode 1))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

(use-package neotree
  :init
  (global-set-key [f5] 'neotree-toggle)
  (global-set-key [f8] 'neotree-refresh))

(use-package pyim
  :init
  (setq default-input-method "pyim")
  (global-set-key (kbd "C-\\") 'toggle-input-method))

(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lsp-mode
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
