;;
;; GUI
;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 0):

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Disable startup messages
(setq initial-scratch-message "")
(setq inhibit-startup-message t)


;;
;; Packages
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


(use-package org
	     :bind (("C-C a" . org-agenda))
	     :init (setq
		    org-agenda-files '("~/src/org/")
		    org-todo-keywords '((sequence
					 "TODO"
					 "IN-PROGRESS"
					 "WAITING"
					 "|"
					 "DONE"
					 "CANCELED"))
		    org-tag-alist '((:startgroup)
				    ("@home" . ?h)
				    ("@office" . ?o)
				    (:endgroup)
				    ("errand" . ?e)
				    ("computer" . ?c)
				    ("phone" . ?p))
		    org-startup-folded 'content
		    org-startup-indented t))

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
	    (call-process "xrandr" nil nil nil "--output" default-output "--auto")
	  (call-process
	   "xrandr" nil nil nil
	   "--output" (match-string 1) "--primary" "--auto"
	   "--output" default-output "--off")
	  (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

  (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
  (exwm-randr-enable)

  (require 'exwm-config)
  (exwm-config-default)

  (setq exwm-input-global-keys
	`(
	  ;; 's-r': Reset (to line-mode).
	  ([?\s-r] . exwm-reset)
	  ;; 's-p': Launch application.
	  ([?\s-p] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
	  ;; 's-N': Switch to certain workspace.
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
  :config
  (setq desktop-environment-volume-get-command "pulsemixer --get-volume"
	desktop-environment-volume-get-regexp "\\([0-9]+\\)"
	desktop-environment-volume-set-command "pulsemixer --change-colume %s"
	desktop-environment-volume-toggle-command "pulsemixer --toggle-mute"
	desktop-environment-volume-normal-increment "+5"
	desktop-environment-volume-normal-decrement "-5"
	desktop-environment-volume-small-increment "+1"
	desktop-environment-volume-small-decrement "-1"
	desktop-environment-screenshot-directory "~/pic"
	desktop-environment-screenshot-command "maim ~/pic/sc_$(date +'%Y-%m-%d-%H%M%S.png')"
	desktop-environment-screenshot-partial-command "maim -s  ~/pic/sc_$(date +'%Y-%m-%d-%H%M%S.png')"))

(use-package moody
  :ensure t
  :pin melpa-stable
  :config
  (setq x-underline-at-descent-line t
	moody-mode-line-height 20)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

;; store custom UI and package-selected-packages in an untracked file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
