;;; init.el -*- lexical-binding: t; -*-

;;; Package Management

;; Configure package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Useful straight.el settings
(with-no-warnings
  (setq straight-base-dir (concat user-emacs-directory "var")
        straight-use-package-by-default 1
        straight-cache-autoloads t
        straight-enable-use-package-integration t
        straight-check-for-modifications '(check-on-save find-when-checking)))

;; Install and/or execute Straight.el for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "var/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; Important Packages

;; Add use-package for package configuration
(straight-use-package 'use-package)

;; Keep the base directory tidy
(use-package no-littering
  :ensure t
  :demand t
  :autoload
  (no-littering-theme-backups)
  :config
  (ignore-errors (make-directory (expand-file-name "auto-save/" user-emacs-directory)))
  (no-littering-theme-backups)
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
    custom-file (no-littering-expand-etc-file-name "custom.el"))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))
    (add-hook 'emacs-startup-hook
      (lambda ()
        (setq recentf-max-menu-items 100
              recentf-max-saved-items 100))))
  (eval-when-compile
    (require 'recentf)))

;;; Bootstrap

;; Tangle and compile if necessary only, then load the configuration
(let ((default-directory user-emacs-directory)
      (read-process-output-max (* 1024 1024)))

  (let* ((config-file (expand-file-name "README" user-emacs-directory))
         (org-config (concat config-file ".org"))
         (tangled-config (concat config-file ".el"))
         (compiled-config (concat config-file ".elc")))

    ;; rebuild the org config if it has been updated recently
    (when (file-newer-than-file-p org-config tangled-config)
      ;; delete any existing byte compiled init file to prevent an outdated version from loading
      ;(when (file-exists-p compiled-config)
      ;      (delete-file compiled-config))
      (require 'org)
      (org-babel-tangle-file org-config)
      ;(byte-compile-file tangled-config)
      ;; then native-compile it
      ;(native-compile tangled-config)
      )

    ;; re-enable messaging
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)

    ;; load our config
    (load config-file)

    )

  ;; Set working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))

;;; init.el ends here
