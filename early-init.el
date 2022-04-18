;;; early-init.el -*- lexical-binding: t; -*-

;; Much of this file is cribbed from doom-emacs

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package initialization
;; via Straight.el, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Change the default location of natively-compiled Files
;; to align with the no-littering package
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
     (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun my/reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)

  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11")

  (when (fboundp 'native-comp-available-p)
      (progn
        (require 'comp)
        (setq package-native-compile t)
        (setq native-comp-deferred-compilation t)
        (setq native-comp-compiler-options '("-O2" "-mtune=native"))
        (custom-set-variables
         '(native-comp-async-report-warnings-errors 'silent))
        ))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (advice-remove #'load-file #'load-file@silence))))


;;; Annoyances

(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
      uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil
      ;; Don't prompt for confirmation when we create a new file or buffer
      ;; (assume the user knows what they're doing).
      confirm-nonexistent-file-or-buffer nil
      ;; middle-click paste at point, not at click
      mouse-yank-at-point t
      read-process-output-max (* 1024 1024) ;; 1mb
      )

;; Avoid to show a message about deprecation of cl package
(setq byte-compile-warnings '(cl-functions))

;; disable the annoying startup message in the echo area
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Emacs disables some commands by default, such as upcase-region, and will
;; ask whether they should be re-enabled. Let's get rid of this annoying prompt.
(setq disabled-command-hook nil)

;; Disable writing package settings to init.el
(defun package--save-selected-packages (&rest opt) nil)

;; Don't add a `custom-set-variables' block to init.el
(setq package--init-file-ensured t)

;; Don't support `customize' operations: it's a clumsy interface
;; that sets variables at a time where it can be easily and unpredictably
;; overwritten. Configure things from README.org instead.
(dolist (sym '(customize-option customize-browse customize-group customize-face
               customize-rogue customize-saved customize-apropos
               customize-changed customize-unsaved customize-variable
               customize-set-value customize-customized customize-set-variable
               customize-apropos-faces customize-save-variable
               customize-apropos-groups customize-apropos-options
               customize-changed-options customize-save-customized))
  (put sym 'disabled "Do not use `customize', configure Emacs from README.org instead"))
(put 'customize-themes 'disabled "Use `load-theme' in README.org instead")


;; Emacs is in love with showing you its NEWS file; it’s bound to like
;; four different keybindings. Overriding the function makes it a
;; no-op. You might say... no news is good news.
(defalias 'view-emacs-news 'ignore)

;; For that matter, we can also elide more GNU agitprop.
(defalias 'describe-gnu-project 'ignore)

;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      )


;;; Cursor

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Don't show the cursor in windows other than selected
(setq cursor-in-non-selected-windows nil)


;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; With this option, changing a frame’s font, menu bar, tool bar, internal
;; borders, fringes or scroll bars will not resize its outer frame to keep
;; the number of columns or lines of its text area unaltered.
(setq frame-inhibit-implied-resize t)

;; Disable tool, menu, and scrollbars. Doom is designed to be keyboard-centric,
;; so these are just clutter (the scrollbar also impacts performance). Whats
;; more, the menu bar exposes functionality that Doom doesn't endorse.
;;
;; I am intentionally not calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because they do extra and unnecessary work that can be more
;; concisely and efficiently expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; GUIs are inconsistent across systems and themes (and will rarely match our
;; active Emacs theme). They impose inconsistent shortcut key paradigms too.
;; It's best to avoid them altogether and have Emacs handle the prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Favour vertical splits over horizontal ones.
(setq split-width-threshold 160
      split-height-threshold nil)


;;; Minibuffer

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area...
(setq resize-mini-windows 'grow-only
      ;; ...but don't let it grow too big
      max-mini-window-height 30)

;; Typing yes/no is obnoxious when y/n will do
(setq use-short-answers t)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; Fonts

;; Set the preferred default font early
(add-to-list 'default-frame-alist
             '(font . "SFMono Nerd Font:pixelsize=10:weight=normal:slant=normal:width=normal:spacing=100:scalable=true:hinting=true"))

;; Disable italics because they can be hard to read
(set-face-italic-p 'italic nil)


;;; Modeline

;; Since the modeline will be replaced with a neater package,
;; we can remove it early.
(setq-default mode-line-format nil)


;;; Networking

;; Improve security when collecting packages
(setq tls-checktrust t
      gnutls-verify-error t)


;;; Bootstrap

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))
