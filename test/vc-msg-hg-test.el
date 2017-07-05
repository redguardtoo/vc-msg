(require 'ert)
(require 'vc-msg-hg)

(defun vc-msg-hg-blame-output (cmd)
  "40660df906a2: hello world
40660df906a2: bye world
40660df906a2: See you again world
02c0a1830e6c: another line
02c0a1830e6c:")

(defun vc-msg-hg-changelist-output (id)
  "changeset:   0:40660df906a2
user:        chen bin <chenbin@mymail.com>
date:        Mon Jun 26 22:14:52 2017 +1000
summary:     first import")


(ert-deftest vc-msg-test-hg ()
  (let* ((info (vc-msg-hg-execute "vc-msg-hg.el" 2))
         msg)
    (should (listp info))
    (should (string= "40660df906a2" (plist-get info :id)))
    (should (string= "chen bin <chenbin@mymail.com>" (plist-get info :author)))
    (should (string= "Mon Jun 26 22:14:52 2017" (plist-get info :author-time)))
    (should (string= "+1000" (plist-get info :author-tz)))
    (setq msg (vc-msg-hg-format info))
    (message "msg=%s" msg)
    (should (string-match-p "^Commit: 40660df9" msg))
    (should (string-match-p "^Author: chen bin <chenbin@mymail.com>" msg))
    (should (string-match-p "Timezone: \\+1000 Sydney" msg))))
