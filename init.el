;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'map)
(require 'subr-x)

;;;; Code:
(setq gc-cons-threshold 100000000)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
;; Don't show the startup message
(setq inhibit-startup-message t)

(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)

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

(setq svarog//default-font "Iosevka-12")

;;;; Look customization
(svarog/defhook look-setup () after-init-hook "Look setup"
		(if window-system
		    (progn
		      (add-to-list 'default-frame-alist `(font . ,svarog//default-font))
		      (set-face-font 'default svarog//default-font)
		      )))

;;;; Svarog keymap
(use-package bind-key
  :demand t)

(defvar svarog//keymap (make-sparse-keymap)
  "Keymap for Svarog prefix commands. Bound under \\[radian-keymap].")

(bind-key* "M-P" svarog//keymap)
(defmacro svarog/bind-key (key-name command &optional predicate)
  "Bind key in `radian-keymap'. KEY-NAME, COMMAND and PREDICATE are as in `bind-key'."
  `(bind-key ,key-name ,command svarog//keymap ,predicate))

(defun svarog/find-init-file () (interactive)
        (find-file "~/.emacs.d/init.el"))

(svarog/bind-key "e i" #'svarog/find-init-file)

;;;; Version control
(use-package git)

(setq vc-handled-backends nil) ; disables build-in version control
(use-package magit
  :demand t)

(defconst svarog/config-local-directory
  (concat user-emacs-directory "config-local/"))
(unless (file-exists-p svarog/config-local-directory)
  (make-directory svarog/config-local-directory))

(defconst svarog/custom-file (expand-file-name "custom.el" svarog/config-local-directory))
(setq custom-file svarog/custom-file)
(load svarog/custom-file 'noerror)

(setq recentf-save-file (expand-file-name "recentf" svarog/config-local-directory))

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

(straight-use-package 'rust-mode)

(straight-use-package 'cquery
		      :config
		      (svarog/defhook enable-lsp () c++-mode-hook "Enable lsp at file open."
				      (lsp-cquery-enable)))

(with-eval-after-load 'projectile
  (projectile-register-project-type 'cpp-code '("projectile-cpp")
                                    :configure "(mkdir build-debug; cd build-debug; cmake ../source -GNinja -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug; cd ..; ln -s build-debug/compile_commands.json)"
                                    :src-dir "source"
                                    :compile "(cd build-debug; cmake --build . )"
                                      :test "(cd build-debug; ctest .)"
                                      :test-suffix ".test.cpp"
                                      ))

(use-package projectile
  :config
  (setq projectile-cache-file (concat svarog/config-local-directory "projectile.cache")
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                         svarog/config-local-directory))
  (projectile-mode +1)
  :blackout t)

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
  (helm-mode t))


(use-package bazel-mode)

;;;; Editing tweaks

(use-package undo-tree
  :demand t
  :blackout t
  :config
  (global-undo-tree-mode))

(electric-indent-mode t)
(setq linum-format " %4d ")
(linum-mode t)

(use-package smartparens
  :demand t
  :config
  (smartparens-mode t)
  (smartparens-global-mode t))

(use-package rainbow-delimiters
  :demand t
  :config
  (rainbow-delimiters-mode t))

(use-package which-key
  :blackout t
  :config
  (which-key-mode))

;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
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
