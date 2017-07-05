#!/bin/sh
cd "$(dirname "$0")"
emacs -batch -L .. -l vc-msg-git-test.el -f ert-run-tests-batch-and-exit
emacs -batch -L .. -l vc-msg-p4-test.el -f ert-run-tests-batch-and-exit
emacs -batch -L .. -l vc-msg-hg-test.el -f ert-run-tests-batch-and-exit
emacs -batch -L .. -l vc-msg-svn-test.el -f ert-run-tests-batch-and-exit
