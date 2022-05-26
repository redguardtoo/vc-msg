;; vc-msg-tests.el --- unit tests for vc-msg -*- lexical-binding: t; -*-

;; Author: Chen Bin <chenbin.sh@gmail.com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'ert)
(require 'vc-msg-git)
(require 'vc-msg-hg)
(require 'vc-msg-p4)
(require 'vc-msg-svn)

(defun vc-msg-git-shell-output (cmd)
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
  (let* ((info (vc-msg-git-execute "vc-msg-git.el" 4 "99"))
     msg)
  (should (listp info))
  (should (string= "Chen Bin" (plist-get info :author)))
  (should (string= "1498284840" (plist-get info :author-time)))
  (should (string= "+1000" (plist-get info :author-tz)))
  (should (string= "first import" (plist-get info :summary)))

  (setq msg (vc-msg-git-format info))
  ;; timezone in CI server is different
  (should (string-match "Date: Sat Jun 24 [0-9][0-9]:14:00 2017" msg))
  (should (string-match "Timezone: \\+1000 Sydney" msg))))

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
    (should (string-match-p "^Commit: 40660df9" msg))
    (should (string-match-p "^Author: chen bin <chenbin@mymail.com>" msg))
    (should (string-match-p "Timezone: \\+1000 Sydney" msg))))

(defun vc-msg-p4-annotate-output (cmd)
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

(ert-run-tests-batch-and-exit)