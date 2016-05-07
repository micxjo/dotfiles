(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-startup-screen t
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(proof-electric-terminator-enable t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package monokai-theme
  :init
  (setq monokai-use-variable-pitch nil))

(set-face-attribute 'default nil :height 150)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups/") t)))
(setq load-prefer-newer)

(show-paren-mode 1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
(ido-mode t)

(global-set-key (kbd "M-o") 'other-window)
(global-subword-mode)

(let ((local-path (expand-file-name "~/.local/bin")))
  (when (file-exists-p local-path)
    (setenv "PATH" (concat local-path ":" (getenv "PATH")))
    (add-to-list 'exec-path local-path)))
(let ((homebrew-path (expand-file-name "/usr/local/bin")))
  (when (file-exists-p homebrew-path)
    (setenv "PATH" (concat homebrew-path ":" (getenv "PATH")))
    (add-to-list 'exec-path homebrew-path)))

(use-package
  fill-column-indicator
  :commands (fci-mode)
  :config
  (setq fci-rule-width 3
        fci-rule-column 80))

(use-package
 smart-mode-line
 :init
 (sml/setup)
 (setq sml/shorten-directory t
       sml/shorten-modes t))

(use-package
 rich-minority
 :init
 (setq rm-blacklist (mapconcat 'identity
                               (quote (" HI"
                                       " company"
                                       " Paredit"
                                       " Projectile\.\*"))
                               "\\|")))

(use-package
 projectile
 :config
 (projectile-global-mode))

(use-package
  haskell-mode
  :mode "\\.hs\\'"
  :init
  (add-hook 'haskell-mode-hook
            (lambda () (setq show-trailing-whitespace t)))
  :config
  (customize-set-variable 'haskell-process-auto-import-loaded-modules t)
  (customize-set-variable 'haskell-process-suggest-remove-import-lines nil)
  (customize-set-variable 'haskell-process-type 'stack-ghci)
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-l")
    'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "s-b")
    'haskell-mode-jump-to-def))

(use-package
  hindent
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (customize-set-variable 'hindent-style "johan-tibell"))

(use-package
  ghc
  :defer t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package
  alchemist
  :init
  (add-hook 'alchemist-mode-hook
            (lambda () (setq show-trailing-whitespace t))))

(use-package
  paredit
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package
  lua-mode
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 2))

(use-package company
  :config
  (setq company-tooltip-align-annotations t))

(use-package
  rust-mode
  :mode "\\.rs\\'")

(let ((racer-cmd-path (expand-file-name "~/.cargo/bin/racer"))
      (rust-src-path (expand-file-name "~/rust/rust/src")))
  (when (and (file-exists-p racer-cmd-path)
             (file-exists-p rust-src-path))
    (use-package
      racer
      :defer t
      :init
      (setenv "CARGO_HOME" (expand-file-name "~/.cargo"))
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode)
      (add-hook 'racer-mode-hook #'company-mode)
      (setq racer-cmd racer-cmd-path)
      (setq racer-rust-src-path rust-src-path)
      :config
      (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))))

(use-package
  yaml-mode
  :mode "\\.ya?ml\\'")

(use-package
  go-mode
  :mode "\\.go\\'"
  :init
  (add-hook 'go-mode-hook
            (lambda () (setq show-trailing-whitespace t))))

(use-package
  nix-mode
  :mode "\\.nix\\'")

(use-package
  ponylang-mode
  :mode "\\.pony\\'"
  :init
  (add-hook 'ponylang-mode-hook
            (lambda ()
              (set-variable 'tab-width 2))))

(use-package
  magit
  :commands (magit-status
             magit-blame
             magit-checkout
             magit-log-buffer))

(use-package
  toml-mode
  :mode "\\.toml\\'")

;; Follow links to vc-controlled files without asking
(setq vc-follow-symlinks t)

(setq user-email-address "micxjo@fastmail.com"
      user-full-name "Micxjo Funkcio")

(setq require-final-newline t)
