#!/bin/sh
cd "$(dirname "$0")"
emacs -batch -l cl-lib -l ert -l ../vc-msg-sdk.el -l ../vc-msg-git.el -l git.test.el -f ert-run-tests-batch-and-exit
emacs -batch -l cl-lib -l ert -l ../vc-msg-sdk.el -l ../vc-msg-p4.el -l p4.test.el -f ert-run-tests-batch-and-exit
emacs -batch -l cl-lib -l ert -l ../vc-msg-sdk.el -l ../vc-msg-hg.el -l hg.test.el -f ert-run-tests-batch-and-exit
emacs -batch -l cl-lib -l ert -l ../vc-msg-sdk.el -l ../vc-msg-svn.el -l svn.test.el -f ert-run-tests-batch-and-exit
