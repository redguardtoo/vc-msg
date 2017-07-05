(require 'ert)
(require 'vc-msg-svn)

(defun vc-msg-svn-blame-output (cmd)
  "     2         cb hello world")

(defun vc-msg-svn-changelist-output (id)
  "------------------------------------------------------------------------
r2 | cb | 2017-06-29 20:03:50 +1000 (四, 29 6月 2017) | 1 line

first import
------------------------------------------------------------------------")

(ert-deftest vc-msg-test-svn ()
  (let* ((info (vc-msg-svn-execute "vc-msg-svn.el" 2))
         msg)
    (should (listp info))
    (should (string= "2" (plist-get info :id)))
    (should (string= "cb" (plist-get info :author)))
    (should (string= "2017-06-29 20:03:50" (plist-get info :author-time)))
    (setq msg (vc-msg-svn-format info))
    (should (string-match-p "^Commit: 2" msg))
    (should (string-match-p "^Author: cb" msg))
    (should (string-match-p "first import" msg))))
