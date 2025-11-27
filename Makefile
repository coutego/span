EMACS ?= emacs

# Attempt to load packages from user's emacs configuration or assume they are in load-path.
# We initialize package to ensure dependencies like 'f', 'ts', 'dash' are available.
test:
	$(EMACS) -batch \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		--eval "(add-to-list 'load-path \".\")" \
		-l tests/test-persistence.el \
		-l tests/test-domain.el \
		-f ert-run-tests-batch-and-exit
