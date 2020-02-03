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
;; Org Mode
;;

(require 'org)

(setq org-agenda-files '("~/src/org/"))

(setq org-todo-keywords '((sequence
			   "TODO"
			   "IN-PROGRESS"
			   "WAITING"
			   "|"
			   "DONE"
			   "CANCELED")))
(setq org-tag-alist '((:startgroup)
                      ("@home" . ?h)
		      ("@office" . ?o)
		      (:endgroup)
                      ("errand" . ?e)
                      ("computer" . ?c)
                      ("phone" . ?p)))

;; Unfold all headlines, but keep content folded
(setq org-startup-folded 'content)
;; Indent text visually according to outline structure
(setq org-startup-indented t)

(global-set-key (kbd "C-c a") 'org-agenda)
