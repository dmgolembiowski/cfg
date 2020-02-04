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
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;
;; Org Mode
;;

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
