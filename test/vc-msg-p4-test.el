(require 'ert)
(require 'vc-msg-p4)

(defun vc-msg-p4-anonate-output (cmd)
  "208624: import React from 'react';
210582: import classNames from 'classnames';")

(defun vc-msg-p4-changelist-output (id)
  "# A Perforce Change Specification.

Change:	210582

Date:	2016/04/12 09:12:04

Client:	AUS-TEST-MY-001

User:	chenbin

Status:	submitted

Description:
	renaming the file a tofile b
	test file 1
	and
	test file 2")


(ert-deftest vc-msg-test-p4 ()
  (let* ((info (vc-msg-p4-execute "vc-msg-p4.el" 2))
         msg)
    (should (listp info))
    (should (string= "210582" (plist-get info :id)))
    (should (string= "chenbin" (plist-get info :author)))
    (should (string= "2016/04/12 09:12:04" (plist-get info :author-time)))
    (setq msg (vc-msg-p4-format info))
    (should (string-match-p "^Date: 2016/04/12 09:12:04" msg))
    (should (string-match-p "^Author: chenbin" msg))
    ;; line trimmed
    (should (string-match-p "^renaming the file a tofile b" msg))
    ;; get first line id
    (setq info (vc-msg-p4-execute "vc-msg-p4.el" 1))
    (should (string= "208624" (plist-get info :id)))))
