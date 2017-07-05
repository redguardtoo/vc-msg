(require 'ert)
(require 'vc-msg-git)

(defun vc-msg-git-blame-output (cmd)
  "4eec6df8e05d8619a5827fdc10ea853cd0c10b2f 4 4 1
author Chen Bin
author-mail <chenbin@mymail.com>
author-time 1498284840
author-tz +1000
committer Chen Bin
committer-mail <chenbin@mymail.com>
committer-time 1498318225
committer-tz +1000
summary first import
boundary
filename vc-msg-git.el")

(ert-deftest vc-msg-test-git ()
  (let* ((info (vc-msg-git-execute "vc-msg-git.el" 4))
     msg)
  (should (listp info))
  (should (string= "Chen Bin" (plist-get info :author)))
  (should (string= "1498284840" (plist-get info :author-time)))
  (should (string= "+1000" (plist-get info :author-tz)))
  (should (string= "first import" (plist-get info :summary)))

  (setq msg (vc-msg-git-format info))
  (should (string-match-p "Date: Sat Jun 24 16:14:00 2017" msg))
  (should (string-match-p "Timezone: \\+1000 Sydney" msg))))
