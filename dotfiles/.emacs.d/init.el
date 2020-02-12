;;
;; Packages
;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;
;; Core
;;

(use-package emacs
  :custom
  (load-prefer-newer t "Prevent loading stale byte code")
  (visible-bell t "Flash frame to represent bell")
  :config
  (fset 'yes-or-no-p 'y-or-n-p))  ;; y/n in stead of yes/no

(use-package simple
  :custom
  (save-interprogram-paste-before-kill
   t
   "Store clipboard text into kill ring before replacement")
  (column-number-mode t "Display column number in mode line"))


;;
;; GUI
;;

(use-package emacs
  :custom
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (initial-scratch-message nil)
  (inhibit-startup-screen t)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1 "Move point when srolling")
  (default-frame-alist '((font . "IBM Plex Mono-10"))))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package frame
  :custom
  (blink-cursor-mode nil "Disable cursor blinking"))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward "Results in foo/Makefile bar/Makefile")
  (uniquify-ignore-buffers-re "^\\*" "Ignore special buffers")
  (uniquify-after-kill-buffer-p
   t
   "Rename buffer back after killing matching buffers"))


;;
;; History
;;

(use-package saveplace
  :custom
  (save-place-file (concat user-emacs-directory "saveplace")
                   "Keep saved places out of working directories"))

(use-package files
  :config
  (let ((autosave-dir (concat user-emacs-directory
                              "auto-save/")))
    (if (not (file-exists-p autosave-dir))
        (make-directory autosave-dir t))

    (setq auto-save-file-name-transforms
          (append auto-save-file-name-transforms
                  (list (list ".*" autosave-dir t)))))
  :custom
  (backup-directory-alist `(("." . "~/.emacs.d/backups"))
                          "Keep backups out of working directories")
  (backup-by-copying t "Use cp and overwrite of original when making backup")
  (version-control t "Make numberic backup versions of edited files")
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2))

(use-package savehist
  :custom
  (savehist-file "~/.emacs.d/savehist")
  (savehist-additional-variables
   '(search-ring regexp-search-ring)
   "Save search entries in addition to minibuffer entries")
  (savehist-autosave-interval 60 "Decrease autosave interval")
  (savehist-mode 1))

(use-package recentf
  :custom
  (recentf-save-file "~/.emacs.d/recentf")
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup
   'never "Disable auto cleanup since it can cause problems with remote files")
  (recentf-mode 1))

(use-package saveplace
  :custom
  (save-place-file "~/.emacs.d/saveplace")
  (save-place-mode 1))


;;
;; Editor
;;

(use-package emacs
  :custom
  (indent-tabs-mode nil "Indent with spaces as detault")
  (tab-width 8)
  (tab-always-indent
   'complete
   "TAB indents if unindented or completes if already indented")
  (sentence-end-double-space nil))

(use-package files
  :custom
  (require-final-newline t "Add newline at end of file if there isn't one"))

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Auto revert buffers when file change on disk"))

(use-package hippie-expand
  :bind
  ("M-/" . hippie-expand))

(use-package paren
  :custom (show-paren-mode 1))


;;
;; Built-in packages
;;

(use-package org
  :bind (("C-C a" . org-agenda))
  :custom
  (org-agenda-files '("~/src/org/"))
  (org-todo-keywords '((sequence
                        "TODO"
                        "IN-PROGRESS"
                        "WAITING"
                        "|"
                        "DONE"
                        "CANCELED")))
  (org-tag-alist '((:startgroup)
                   ("@home" . ?h)
                   ("@office" . ?o)
                   (:endgroup)
                   ("errand" . ?e)
                   ("computer" . ?c)
                   ("phone" . ?p)))
  (org-startup-folded 'content)
  (org-startup-indented t))

(defun eu/enable-whitespace ()
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (if (derived-mode-p 'org-mode)
      (setq-local whitespace-line-column 200)
    (progn
      (setq-local whitespace-line-column 80)
      (setq-local display-line-numbers t)))
  (whitespace-mode +1)
  (when (derived-mode-p 'makefile-mode)
      (whitespace-toggle-options '(tabs tab-mark))))

(use-package whitespace
  :hook ((text-mode prog-mode) . eu/enable-whitespace)
  :custom
  (whitespace-style '(face tabs tab-mark empty trailing lines-tail)))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package ediff-wind
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain
                               "Default multiframe breaks EXWM"))

(use-package vc-hooks
  :custom (vc-follow-symlinks t "Follow symlink to vc file without asking"))

(use-package dired
  :custom (dired-listing-switches "-alh"))


;;
;; Third-party packages
;;

(use-package which-key
  :ensure t
  :pin melpa-stable
  :custom
  (which-key-mode t))

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind (("C-x g" . magit-status)))

(use-package exwm
  :ensure t
  :config
  (require 'exwm-randr)

  (setq exwm-randr-workspace-monitor-plist
        '(0 "DP-1" 2 "DP-1" 3 "DP-1" 4 "DP-1" 5 "DP-1"))
  (exwm-randr-enable)

  (require 'exwm-config)
  (exwm-config-default)

  (defun eu/xrandr-toggle (arg)
    (call-process (expand-file-name "~/.local/bin/xrandr-toggle")
                  nil nil nil arg))

  (setq exwm-input-global-keys
        `(
          ;; Toggle between char and line mode:
          ([?\s-i] . exwm-input-toggle-keyboard)
          ;; Launch appliction:
          ([?\s-p] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Launch xterm:
          ,`(,(kbd "<S-s-return>") . (lambda ()
                              (interactive)
                              (start-process-shell-command "xterm" nil
                                                           "xterm")))
          ;; Switch to external display:
          ,`(,(kbd "<XF86Display>") . (lambda ()
                                        (interactive)
                                        (eu/xrandr-toggle "external")))
          ;; Switch to internal display:
          ,`(,(kbd "M-<XF86Display>") . (lambda ()
                                          (interactive)
                                          (eu/xrandr-toggle "internal")))
          ;; Switch to internal and external display:
          ,`(,(kbd "C-<XF86Display>") . (lambda ()
                                          (interactive)
                                          (eu/xrandr-toggle "both")))
          ;; Switch to certain workspace N:
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (eu/xrandr-toggle "internal")

  (setq window-divider-default-bottom-width 2
        window-divider-default-right-width 2)
  (window-divider-mode))

(use-package desktop-environment
  :ensure t
  :pin melpa-stable
  :init
  (desktop-environment-mode)
  :custom
  (desktop-environment-volume-get-command "pulsemixer --get-volume")
  (desktop-environment-volume-get-regexp "\\([0-9]+\\)")
  (desktop-environment-volume-set-command "pulsemixer --change-colume %s")
  (desktop-environment-volume-toggle-command "pulsemixer --toggle-mute")
  (desktop-environment-volume-normal-increment "+5")
  (desktop-environment-volume-normal-decrement "-5")
  (desktop-environment-volume-small-increment "+1")
  (desktop-environment-volume-small-decrement "-1")
  (desktop-environment-screenshot-directory "~/pic")
  (desktop-environment-screenshot-command
   "maim ~/pic/sc_$(date +'%Y-%m-%d-%H%M%S.png')")
  (desktop-environment-screenshot-partial-command
   "maim -s  ~/pic/sc_$(date +'%Y-%m-%d-%H%M%S.png')"))

(use-package diff-hl
  :ensure t
  :pin melpa-stable
  :hook
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (fringe-mode 8) ;; reset fringe mode to default
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package hl-todo
  :ensure t
  :pin melpa-stable
  :config
  (global-hl-todo-mode 1))

(use-package guru-mode
  :ensure t
  :config
  (guru-global-mode 1))

;;
;; Languages
;;

(use-package make-mode
  :ensure nil
  :hook (makefile-mode . (lambda () (setq indent-tabs-mode t))))

(use-package yaml-mode
  :ensure t
  :pin melpa-stable
  :mode ("\\.yml$" . yaml-mode)
  :hook (yaml-mode . subword-mode)) ;; yaml-mode derives from text-mode

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package gitignore-mode
  :ensure t
  :pin melpa-stable)


;;
;; Customization
;;

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (when (file-exists-p custom-file)
    (load custom-file)))
