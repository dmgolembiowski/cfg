;; init.el --- GNU Emacs configuration file

;;; Commentary:

;; Sets up the essentials for using packages
;; and loading the main configuration in
;; org format.

;;; Code:

(defvar gc-cons-threshold-original gc-cons-threshold)
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold (* 1024 1024 100))
(setq file-name-handler-alist nil)

;;
;; Packages
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-url
       (concat "https://raw.githubusercontent.com"
               "/raxod502/straight.el/develop/install.el"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously bootstrap-url
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(require 'org)
(setq vc-follow-symlinks t)
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;; init.el ends here
