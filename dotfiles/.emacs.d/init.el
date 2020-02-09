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


;;
;; Core
;;

(use-package emacs
  :custom
  (setq load-prefer-newer t "Prevent loading stale byte code")
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
  (scroll-preserve-screen-position 1 "Move point when srolling"))

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
  :custom
  (backup-directory-alist `(("." . "~/.emacs.d/backups"))
                          "Keep backups out of working directories")
  (backup-by-copying t "Use cp and overwrite of original when making backup")
  (version-control t "Make numberic backup versions of edited files")
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2))

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
   "TAB indents if unindented or completes if already indented"))

(use-package files
  :custom
  (require-final-newline t "Add newline at end of file if there isn't one"))

(use-package autorevert
  :custom
  (global-auto-revert-mode t "Auto revert buffers when file change on disk"))

(use-package hippie-expand
  :bind
  ("M-/" . hippie-expand))

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
  (whitespace-mode +1))

(use-package whitespace
  :hook ((text-mode prog-mode) . eu/enable-whitespace)
  :custom
  (whitespace-line-column 80)
  (whitespace-style '(face tabs tab-mark empty trailing lines-tail)))


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

  (defun exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          default-output)
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
            (call-process "xrandr" nil nil nil
                          "--output" default-output "--auto")
          (call-process "xrandr" nil nil nil
                        "--output" (match-string 1) "--primary" "--auto"
                        "--output" default-output "--off")
          (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

  (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
  (exwm-randr-enable)

  (require 'exwm-config)
  (exwm-config-default)

  (setq exwm-input-global-keys
        `(
          ;; Toggle between char and line mode:
          ([?\s-i] . exwm-input-toggle-keyboard)
          ;; Launch appliction:
          ([?\s-p] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ;; Launch appliction:
          ;; FIMXE: ([?\s-s-return] . (start-process-shell-command "xterm" nil "xterm"))
          ;; Switch to certain workspace N:
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

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


;; store custom UI and package-selected-packages in an untracked file
(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (when (file-exists-p custom-file)
    (load custom-file)))
