;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;; INITIAL STATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This starts the Emacs server when .emacs gets loaded
;;
(require 'server)
(if (not (server-running-p)) (server-start))

(setq package-enable-at-startup nil)

;; Bootstrap 'straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package '(org :type built-in))

;; Move customization variables to a separate file and load it
(setq custom-file "~/.emacs.d/custom-vars.el")
(load custom-file)

;; Load myinit.org
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.straight.org"))
