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
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp track))))
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
(let ((homebrew-path (expand-file-name "~/homebrew/bin")))
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
 (add-hook 'haskell-mode-hook #'hindent-mode)
 (add-hook 'haskell-mode-hook 'company-mode)
 (add-hook 'haskell-mode-hook
           (lambda () (setq show-trailing-whitespace t)))
 (customize-set-variable 'haskell-process-auto-import-loaded-modules t)
 (customize-set-variable 'haskell-process-suggest-remove-import-lines t)
 (customize-set-variable 'haskell-process-type 'stack-ghci)
 (customize-set-variable 'hindent-style "chris-done")
 (eval-after-load 'haskell-mode
   '(progn
      (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
      (define-key haskell-mode-map (kbd "C-c C-l")
        'haskell-process-load-or-reload)
      (define-key haskell-mode-map (kbd "SPC")
        'haskell-mode-contextual-space))))

(use-package
  ghc
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
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

(use-package magit)

;; Keep NickServ passwords out of dotfiles repo
(when (file-exists-p "~/.ercpass")
  (load "~/.ercpass"))

(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq whitespace-global-modes '(not erc-mode))
(setq erc-keywords '("micxjo"))
(erc-match-mode)

(setq erc-log-insert-log-on-open nil
      erc-log-channels t
      erc-log-channels-directory "~/.irclogs/"
      erc-save-buffer-on-part t
      erc-hide-timestamps nil)

(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

(erc-scrolltobottom-mode)
(setq erc-input-line-position -2)
(add-hook 'erc-insert-post-hook 'erc-scroll-to-bottom)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(defun start-irc ()
  "Connect to IRC"
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 6697
           :nick "micxjo"
           :full-name "micxjo"))

(setq user-email-address "micxjo@fastmail.com"
      user-full-name "Micxjo Funkcio")

(setq gnus-select-method
      '(nnmaildir "fastmail"
                  (directory "~/Maildir")
                  (directory-files nnheader-directory-files-safe)
                  (get-new-mail nil)))

(use-package
  elfeed
  :init
  (global-set-key (kbd "C-x w") 'elfeed)
  (when (file-exists-p "~/.elfeeds")
    (load "~/.elfeeds")))
