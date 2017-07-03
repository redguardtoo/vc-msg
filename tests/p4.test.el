;;; p4.test.el --- test vc-msg-p4.el

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

;;; Code:

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
