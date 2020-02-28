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

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(unless (bound-and-true-p package--initialized)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(require 'org)
(set vc-follow-symlinks t)
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;; init.el ends here
