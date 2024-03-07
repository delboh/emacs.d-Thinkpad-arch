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

;; (require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("org" . "http://orgmode.org/elpa/")))

;; (package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Bootstrap 'straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; It is recommended to put (straight-use-package 'org) early in the config.
;; Ideally, right after the straight.el bootstrap.

(straight-use-package 'org)


;; Load myinit.org
(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.straight.org"))

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
