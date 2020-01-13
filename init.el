;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'url-vars)

(if (getenv "HTTP_PROXY")
    (add-to-list 'url-proxy-services `("http" . ,(getenv "HTTP_PROXY"))))
(if (getenv "HTTPS_PROXY")
    (add-to-list 'url-proxy-services `("https" . ,(getenv "HTTPS_PROXY"))))
(if (getenv "FTP_PROXY")
    (add-to-list 'url-proxy-services `("ftp" . ,(getenv "FTP_PROXY"))))
(if (getenv "RSYNC_PROXY")
    (add-to-list 'url-proxy-services `("rsync" . ,(getenv "RSYNC_PROXY"))))
(if (getenv "NO_PROXY")
    (add-to-list 'url-proxy-services `("no_proxy" . ,(getenv "NO_PROXY"))))

(when window-system
  (blink-cursor-mode 0)                          ; Disable the cursor blinking
  (scroll-bar-mode 0)                            ; Disable the scroll bar
  (tool-bar-mode 0)                              ; Disable the tool bar
  (tooltip-mode 0)                               ; Disable the tooltips
  (menu-bar-mode 0))

;;;; Code:
(setq-default
 gc-cons-threshold most-positive-fixnum
 make-backup-files nil                  ; Stop creating backup~ files.
 auto-save-default nil                  ; Stop creating #autosave# files.
 help-window-select t                   ; Focus new help windows when opened.
 indent-tabs-mode nil                   ; Don't use tabs to indent.
 tab-width 2
 inhibit-startup-screen t               ; Disable start-up screen.
 initial-scratch-message ""
 delete-selection-mode t                ; Delete marked text on typing.
 inhibit-startup-message t
 python-intent-offset 4
 vc-handled-backends nil                ; Disables build-in version control.
 )

;;; We need to call the `delete-selection-mode' function here;
;;; only setting the variable does not seem to have any effect.
(delete-selection-mode)

;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'focus-out-hook 'garbage-collect)

(fset 'yes-or-no-p 'y-or-n-p)                    ; Replace yes/no prompts with y/n
(global-visual-line-mode t) ; Soft wrap long lines.
(setq winner-dont-bind-my-keys t)
(winner-mode 1)
(global-set-key (kbd "C-c u") 'winner-undo)
(global-set-key (kbd "C-c r") 'winner-redo)

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;;;  package.el
;;; so package-list-packages includes them
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defmacro svarog/defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Svarog: no docstring provided for `svarog/defadvice'"))
  `(progn
     (eval-and-compile
       (defun ,name ,arglist
         ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                             "an"
                           "a")))
            (format "%s\n\nThis is %s `%S' advice for `%S'."
                    docstring article where
                    (if (and (listp place)
                             (memq (car place) ''function))
                        (cadr place)
                      place)))
         ,@body))
     (advice-add ',place ',where #',name)
     ',name))

(defmacro svarog/defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Svarog: no docstring provided for `svarog/defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)

(straight-use-package 'use-package)

;; Package `blackout' provides a convenient function for customizing
;; mode lighters. It supports both major and minor modes with the same
;; interface, and includes `use-package' integration. The features are
;; a strict superset of those provided by similar packages `diminish',
;; `delight', and `dim'.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;;;; Look customization
(setq svarog//default-font "Iosevka-12")

(svarog/defhook look-setup () after-init-hook "Look setup"
                (if window-system
                    (progn
                      (add-to-list 'default-frame-alist `(font . ,svarog//default-font))
                      (set-face-font 'default svarog//default-font))))

;;;; Svarog keymap
(use-package bind-key
  :demand t)

(defvar svarog//keymap (make-sparse-keymap)
  "Keymap for Svarog prefix commands. Bound under \\[svarog//keymap].")

(bind-key* "M-p" svarog//keymap)
(defmacro svarog/bind-key (key-name command &optional predicate)
  "Bind key in `radian-keymap'. KEY-NAME, COMMAND and PREDICATE are as in `bind-key'."
  `(bind-key ,key-name ,command svarog//keymap ,predicate))

(defun svarog/find-init-file () (interactive)
       (find-file "~/.emacs.d/init.el"))

(svarog/bind-key "e i" #'svarog/find-init-file)

(define-key global-map [(control x)(control b)] (function ibuffer))

;;;; Version control
(use-package git)

(use-package magit
  :demand t
  :config
  (defun exordium-magit-log ()
    "If in `dired-mode', call `magit-dired-log'. Otherwise call
`magit-log-current (or `magit-log' if former not present)."
    (interactive)
    (if (eq 'dired-mode major-mode)
        (call-interactively 'magit-dired-log)
      (if (fboundp 'magit-log-current)
          (call-interactively 'magit-log-current)
        (call-interactively 'magit-log))))
  ;; Keys
  (define-prefix-command 'exordium-git-map nil)
  (define-key exordium-git-map (kbd "s") (function magit-status))
  (define-key exordium-git-map (kbd "l") 'exordium-magit-log)
  (define-key exordium-git-map (kbd "f")
    (if (fboundp 'magit-log-buffer-file)
        (function magit-log-buffer-file)
      (function magit-file-log)))
  (define-key exordium-git-map (kbd "b")
    (if (fboundp 'magit-blame)
        (function magit-blame)
      (function magit-blame-mode)))
  (global-set-key (kbd "C-c g") 'exordium-git-map)
  ;; Make `magit-status' run alone in the frame, and then restore the old window
  ;; configuration when you quit out of magit.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  ;; Make `magit-log' run alone in the frame, and then restore the old window
  ;; configuration when you quit out of magit.
  (defadvice magit-log (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  ;; Don't show "MRev" in the modeline
  (when (bound-and-true-p magit-auto-revert-mode)
    (blackout 'magit-auto-revert-mode))
  )

(defconst svarog/config-local-directory
  (concat user-emacs-directory "config-local/"))
(unless (file-exists-p svarog/config-local-directory)
  (make-directory svarog/config-local-directory))

(defconst svarog/custom-file (expand-file-name "custom.el" svarog/config-local-directory))
(setq custom-file svarog/custom-file)
(load svarog/custom-file 'noerror)

(setq recentf-save-file (expand-file-name "recentf" svarog/config-local-directory))

;; (use-package graphene-meta-theme
;;   :demand t)

(use-package solarized-theme
  :config
  (setq solarized-distinct-fringe-background t
        solarized-use-variable-pitch nil
        solarized-high-contrast-mode-line t
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))
(load-theme 'solarized-dark t)
;; (load-theme 'graphene-meta t)

(use-package powerline
  :demand t
  :config
  (powerline-default-theme))

(defun svarog/reload-theme (theme)
  "Reloads a THEME."
  (load-theme theme)
  (powerline-reset))

(defun svarog/light-theme ()
  "Set solarized light theme."
  (interactive)
  (svarog/reload-theme 'solarized-light))

(defun svarog/dark-theme ()
  "Set solarized dark theme."
  (interactive)
  (svarog/reload-theme 'solarized-dark))

(use-feature uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-feature windmove
  :demand t
  :config
  (global-set-key [(control c) (left)] (function windmove-left))
  (global-set-key [(control c) (right)] (function windmove-right))
  (global-set-key [(control c) (up)] (function windmove-up))
  (global-set-key [(control c) (down)] (function windmove-down))

  (defun move-buffer-up ()
    "Move the current window up, if possible."
    (interactive)
    (let ((other-win (windmove-find-other-window 'up))
          (buffer (window-buffer (selected-window))))
      (cond (other-win
             (set-window-buffer (selected-window) (window-buffer other-win))
             (set-window-buffer other-win buffer)
             (select-window other-win))
            (t
             (message "No window up from selected window!")))))
  (global-set-key [(control c) (shift up)] (function move-buffer-up))

  (defun move-buffer-down ()
    "Move the current window down, if possible."
    (interactive)
    (let ((other-win (windmove-find-other-window 'down))
          (buffer (window-buffer (selected-window))))
      (cond ((and other-win
                  (not (window-minibuffer-p other-win)))
             (set-window-buffer (selected-window) (window-buffer other-win))
             (set-window-buffer other-win buffer)
             (select-window other-win))
            (t
             (message "No window down from selected window")))))
  (global-set-key [(control c) (shift down)]  (function move-buffer-down))

  (defun move-buffer-left ()
    "Move the current window to the left, if possible."
    (interactive)
    (let ((other-win (windmove-find-other-window 'left))
          (buffer    (window-buffer (selected-window))))
      (cond (other-win
             (set-window-buffer (selected-window) (window-buffer other-win))
             (set-window-buffer other-win buffer)
             (select-window other-win))
            (t
             (message "No window left from selected window")))))
  (global-set-key [(control c) (shift left)]  (function move-buffer-left))

  (defun move-buffer-right ()
    "Move the current window to the right, if possible."
    (interactive)
    (let ((other-win (windmove-find-other-window 'right))
          (buffer    (window-buffer (selected-window))))
      (cond (other-win
             (set-window-buffer (selected-window) (window-buffer other-win))
             (set-window-buffer other-win buffer)
             (select-window other-win))
            (t
             (message "No window right from selected window")))))
  (global-set-key [(control c) (shift right)] (function move-buffer-right)))

;;;; Projects config

;; (with-eval-after-load 'projectile
;;   (projectile-register-project-type 'cpp-code '("projectile-cpp")
;;                                     :configure "(mkdir build-debug; cd build-debug; cmake ../source -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug; cd ..; ln -s build-debug/compile_commands.json)"
;;                                     :src-dir "source"
;;                                     :compile "(cd build-debug; cmake --build . )"
;;                                       :test "(cd build-debug; ctest .)"
;;                                       :test-suffix ".test.cpp"
;;                                       ))

(use-package projectile
  :config
  (setq projectile-cache-file (concat svarog/config-local-directory "projectile.cache")
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                         svarog/config-local-directory)
        projectile-completion-system 'helm)
  (projectile-mode +1)
  :blackout t)

(use-package helm-projectile
  :config (helm-projectile-on)
  :blackout t)

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 0.5
  :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . company-select-previous)
         ("<down>" . company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 1)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (global-company-mode +1)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

;;; C++ config:
(svarog/defhook c-mode-common-configuration () c-mode-common-hook
                "Common conf for C mode."
                (c-set-style "bsd")
                (setq c-basic-offset 4
                      tab-width 4
                      indent-tabs-mode nil
                      c-tab-always-indent t
                      c-echo-syntactic-information-p t)
                (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

                (auto-revert-mode t)
                (blackout 'auto-revert-mode)
                (toggle-truncate-lines t))

(svarog/defhook c++-mode-configuration () c++-mode-hook
                "Custom C++ indent config."
                (add-to-list 'c-offsets-alist '(innamespace . 0))
                (c-set-offset 'substatement-open 0)
                (c-set-offset 'label '+))

(use-feature rtags
  :demand t
  :config
  (rtags-enable-standard-keybindings))

(use-package rust-mode)

(display-battery-mode t)

(use-package ledger-mode)

(use-package helm
  :demand t
  :blackout t
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)

  (svarog/defhook
      helm-hide-minibuffer-maybe () helm-minibuffer-set-up-hook
      "Hide minibuffer in Helm session if we use the header line as input field."
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode t)

  (helm-mode t))


(use-package bazel-mode
  :demand t
  :config
  (add-to-list 'auto-mode-alist '("/BUILD\\(\\..*\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("/WORKSPACE\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(BUILD\\|WORKSPACE\\|bzl\\)\\'" . bazel-mode))
  (svarog/defhook autoformat-on-save () bazel-mode-hook "Autoformat on save."
                  (add-hook 'before-save-hook #'bazel-format nil t)))

(use-package protobuf-mode)

;;;; Editing tweaks

(use-package undo-tree
  :demand t
  :blackout t
  :config
  (global-undo-tree-mode))

(use-package nlinum
  :demand t
  :config
  (setq nlinum-highlight-current-line t)
  (global-nlinum-mode))

(svarog/defhook goodies-for-prog-mode () prog-mode-hook "Goodies for prog-mode."
                (electric-indent-mode t)
                ;; (setq linum-format " %4d ")
                ;; (linum-mode t)
                (auto-revert-mode t)
                (setq whitespace-line-column 80)
                (setq whitespace-style '(face tabs empty trailing newline))
                (whitespace-mode t)
                (blackout 'whitespace-mode)
                (hl-line-mode t)
                (hi-lock-mode t)
                (hs-minor-mode t)
                (blackout 'hs-minor-mode))


(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.
  (use-feature org
    :config
    (add-to-list 'sp-ignore-modes-list #'org-mode))
  (use-feature org-agenda
    :config
    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (when (version<= "27" emacs-version)
    (dolist (fun '(c-electric-paren c-electric-brace))
      (add-to-list 'sp--special-self-insert-commands fun)))
  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (radian--smartparens-pair-setup #'prog-mode "(")
  (radian--smartparens-pair-setup #'prog-mode "[")
  (radian--smartparens-pair-setup #'prog-mode "{")
  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'latex-mode "\\[")
  (radian--smartparens-pair-setup #'markdown-mode "```")

  ;; It's unclear to me why any of this is needed.
  (radian--smartparens-pair-setup #'json-mode "[")
  (radian--smartparens-pair-setup #'json-mode "{")
  (radian--smartparens-pair-setup #'tex-mode "{")

  ;; Deal with `protobuf-mode' not using `define-minor-mode'.
  (radian--smartparens-pair-setup #'protobuf-mode "{")

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  :blackout t)

(use-package rainbow-delimiters
  :demand t
  :config
  (svarog/defhook
      add-rainbow-delimiters ()
    prog-mode-hook
    "Add rainbow delimiters to prog mode."
    (rainbow-delimiters-mode t)))

(use-package which-key
  :blackout t
  :demand t
  :config
  (which-key-mode))

(use-package helm-swoop
  :bind ("C-S-s" . (function helm-swoop)))

(use-package helm-ag
  :bind ("C-S-r" . helm-ag-project-root))

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-ff-fuzzy-matching t
      helm-ag-fuzzy-match t
      helm-buffer-details-flag nil
      helm-ag-insert-at-point 'symbol)

(use-package avy
  :bind ("C-." . avy-goto-word-or-subword-1))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-feature bind-key)
(bind-key* "M-o" 'ace-window)

(use-package multi-term
  :bind ("C-t" . multi-term-dedicated-toggle))

(use-package helm-mt)

;; Better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Apply syntax highlighting to all buffers.
(global-font-lock-mode t)

(blackout 'company-mode)
(blackout 'visual-line-mode)
(blackout 'which-key-mode)
(blackout 'auto-revert-mode)
(blackout 'smartparens-mode)
(blackout 'eldoc-mode)
(blackout 'lsp-mode)
(blackout 'whitespace-mode)
(blackout 'hs-minor-mode)
(blackout 'abbrev-mode)

(server-start)
