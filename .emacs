(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
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

(use-package monokai-theme)

(set-face-attribute 'default nil :height 150)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq load-prefer-newer)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(ido-mode t)

(let ((local-path (expand-file-name "~/.local/bin")))
  (when (file-exists-p local-path)
    (setenv "PATH" (concat local-path ":" (getenv "PATH")))
    (add-to-list 'exec-path local-path)))
(let ((homebrew-path (expand-file-name "/usr/local/bin")))
  (when (file-exists-p homebrew-path)
    (setenv "PATH" (concat homebrew-path ":" (getenv "PATH")))
    (add-to-list 'exec-path homebrew-path)))

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
 :init
 (projectile-global-mode))

(use-package company)

(use-package
 haskell-mode
 :init
 (add-hook 'haskell-mode-hook 'company-mode)
 (add-hook 'haskell-mode-hook
           (lambda () (setq show-trailing-whitespace t)))
 (customize-set-variable 'haskell-process-auto-import-loaded-modules t)
 (customize-set-variable 'haskell-process-suggest-remove-import-lines t)
 (customize-set-variable 'haskell-process-type 'stack-ghci)
 (eval-after-load 'haskell-mode
   '(progn
      (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
      (define-key haskell-mode-map (kbd "C-c C-l")
        'haskell-process-load-or-reload)
      (define-key haskell-mode-map (kbd "s-b")
        'haskell-mode-jump-to-def))))

(use-package
  hindent
  :init
  (customize-set-variable 'hindent-style "johan-tibell")
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package
  ghc
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(use-package
  company-ghc
  :init
  (add-to-list 'company-backends 'company-ghc))

(use-package
 alchemist
 :init
 (add-hook 'alchemist-mode-hook
           (lambda () (setq show-trailing-whitespace t))))

(use-package
  paredit
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package
  geiser
  :init
  (add-hook 'geiser-mode-hook
           (lambda () (setq show-trailing-whitespace t)))
  (let ((mac-racket-path "/Applications/Racket v6.3/bin/racket"))
    (if (file-exists-p mac-racket-path)
        (setq geiser-racket-binary mac-racket-path))))

(use-package
  lua-mode
  :init
  (setq lua-indent-level 2))

(let ((coq-path "/Applications/CoqIDE_8.4pl5.app/Contents/Resources/bin"))
  (when (file-exists-p coq-path)
    (setenv "PATH" (concat coq-path ":" (getenv "PATH")))
    (add-to-list 'exec-path coq-path)))

(let ((pg-path
       (expand-file-name "~/coq/ProofGeneral-4.2/generic/proof-site.el")))
  (when (file-exists-p pg-path)
    (load-file pg-path)
    (add-hook 'proof-ready-for-assistant-hook
              (lambda () (show-paren-mode 0)))
    (setq coq-prog-args '("-R" "." "SF"))))

(use-package rust-mode)

(let ((racer-cmd-path (expand-file-name "~/.multirust/cargo/bin/racer"))
      (rust-src-path (expand-file-name "~/rust/rust/src")))
  (when (and (file-exists-p racer-cmd-path)
             (file-exists-p rust-src-path))
    (use-package
      racer
      :init
      (setq racer-cmd racer-cmd-path)
      (setq racer-rust-src-path rust-src-path)
      (add-hook 'rust-mode-hook #'racer-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode)
      (add-hook 'racer-mode-hook #'company-mode))))

(use-package yaml-mode)

(use-package
  go-mode
  :init
  (add-hook 'go-mode-hook
            (lambda () (setq show-trailing-whitespace t))))

(use-package idris-mode)

(use-package nix-mode)

(use-package magit)

(setq user-email-address "micxjo@fastmail.com"
      user-full-name "Micxjo Funkcio")

(setq require-final-newline t)
