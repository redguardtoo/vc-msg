;;; vc-msg.el --- Show Version Control Software (VCS) commit message of current line

;; Copyright (C) 2017 Chen Bin
;;
;; Version: 0.0.1
;; Keywords: git vcs svn hg messenger
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/vc-msg
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'popup)

(defvar vc-msg-force-vcs nil
  "Extra VCS overrides result of `vc-msg-detect-vcs-type'.
It's a string like 'git' or 'svn'.  If it's know `nil', it is
used as key to lookup `vc-msg-plugins'")

(defvar vc-msg-known-vcs
  '(("git" . ".git")
    ("svn" . ".svn")
    ("hg" . ".hg"))
  "List of know VCS.
In VCS, the key like 'git' or 'svn' is used to locate plugin
in `vc-msg-plugins'.  The directory name like '.git' or '.svn'
is used to locate VCS root directory.")

(defvar vc-msg-show-at-line-beginning-p t
  "Show the mesesage at beginning of line.")

(defvar vc-msg-plugins nil
  "List of VCS plugins.
A plugin is a `plist':

  (defun my-execute (file line &optional extra))
  (defun my-format (info))
  (add-to-list 'vc-msg-plugins
               '(:type \"git\"
                :execute my-execute
                :format my-format)

When `vc-msg-show' is running, it does one thing:

  (popup-tip (my-format (my-execute buffer-file-name (line-number-at-pos)))).

The result of `my-execute' is plugin internals unless it's a string or `nil'.
If the result is string or `nil', we assume `my-execute' has failed.
The string result is the extra error message.
Please check `vc-msg-git-execute' and `vc-msg-git-format' for sample.")

(defvar vc-msg-newbie-friendly-p t
  "Show extra friendly hint for newbies.")

(defun vc-msg-detect-vcs-type ()
  "Return VCS type or nil."
  (cond
   ;; use `vc-msg-force-vcs' if it's not nil
   (vc-msg-force-vcs
    vc-msg-force-vcs)
   (t
    ;; or some "smart" algorithm will figure out the correct VCS
    (if (listp vc-msg-known-vcs)
        (cl-some (lambda (e)
                   (if (locate-dominating-file default-directory (cdr e))
                       (car e)))
                 vc-msg-known-vcs)))))

;;;###autoload
(defun vc-msg-init-plugins ()
  (interactive)
  (add-to-list 'vc-msg-plugins
               '(:type "git" :execute vc-msg-git-execute :format vc-msg-git-format)))

;;;###autoload
(defun vc-msg-close ()
  (interactive)
  (throw 'vc-msg-loop t))

(defvar vc-msg-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    (define-key map (kbd "q") 'vc-msg-close)
    map)
  "Keymap of vc-msg popup.")

(defun vc-msg-show-position ()
  (if vc-msg-show-at-line-beginning-p
      (line-beginning-position)
    (point)))

;;;###autoload
(defun vc-msg-show ()
  "Show commit messeage of current line"
  (interactive)
  (let* (finish
         (current-vcs-type (vc-msg-detect-vcs-type))
         (plugin (cl-some (lambda (e)
                            (if (string= (plist-get e :type) current-vcs-type) e))
                          vc-msg-plugins)))
    (when plugin
      ;; load the plugin in run time
      (let* ((plugin-file (intern (concat "vc-msg-" (plist-get plugin :type)))))
        (unless (featurep plugin-file)
          (require plugin-file)))

      (let* ((executer (plist-get plugin :execute))
             (formatter (plist-get plugin :format))
             (commit-info (funcall executer
                                   (file-name-nondirectory buffer-file-name)
                                   (line-number-at-pos)))

             message)
        (cond
         ((listp commit-info)
          ;; we get valid information
          (setq message (funcall formatter commit-info))
          (if vc-msg-newbie-friendly-p
              (setq message (format "%s\n\n%s"
                                    message
                                    "Press q to quit")))
          (while (not finish)
            (let* ((menu (popup-tip message :point (vc-msg-show-position) :nowait t)))
              (unwind-protect
                  (setq finish (catch 'vc-msg-loop
                                 (popup-menu-event-loop menu vc-msg-map 'popup-menu-fallback
                                                        :prompt "[q]uit")
                                 t))
                (popup-delete menu))))
          )
         ((stringp commit-info)
          (message (format "Shell command failed:\n%s"
                           commit-info)))
         (t
          (message "Shell command failed.")))))))

(vc-msg-init-plugins)
(provide 'vc-msg)
;;; vc-msg.el ends here
