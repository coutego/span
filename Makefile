EMACS ?= emacs

.PHONY: test

# We configure package.el to use MELPA and ensure dependencies (f, ts, dash) are installed.
test:
	$(EMACS) -batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (and (package-installed-p 'f) (package-installed-p 'ts) (package-installed-p 'dash)) \
				  (package-refresh-contents) \
				  (dolist (pkg '(f ts dash)) \
				    (unless (package-installed-p pkg) (package-install pkg))))" \
		--eval "(add-to-list 'load-path \".\")" \
		-l tests/test-eli.el \
		-l tests/test-org-chronos-persistence.el \
		-l tests/test-org-chronos-event-log.el \
		-l tests/test-org-chronos-task-linker.el \
		-f ert-run-tests-batch-and-exit
