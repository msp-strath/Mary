SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')

.PHONY: clean install-hasktags TAGS bash-completion

# From https://stackoverflow.com/questions/18649427/using-a-makefile-with-cabal
# If 'cabal install' fails in building or installing, the
# timestamp on the build dir 'dist' may still be updated.  So,
# we set the timestamp on the build dir to a long time in the past
# with 'touch --date "@0"' in case cabal fails.
CABAL_INSTALL = \
  cabal install --overwrite-policy=always \
  || { touch --date "@0" dist ; \
       exit 42 ; }

install: $(SOURCE)
	$(CABAL_INSTALL)
	ln -sf `which mary` .
ifeq ("$(wildcard ./config.php)","")
	@echo "No config.php found, creating one from config.php.sample"
	cp ./config.php.sample ./config.php
endif

clean:
	rm -rf dist dist-newstyle TAGS

install-hasktags:
	cabal update
	cabal install hasktags

.PHONY: test-all test test-mary test-mary-all test-js test-shonkier
test-all:
	cabal new-run mary-tests -- -i

test:
	cabal new-run mary-tests -- -i --regex-exclude "dot"
#	runhaskell -itest test/Test/Main.hs -i

test-mary:
	cabal new-run mary-tests -- -i -p Mary --regex-exclude "dot"

test-mary-all:
	cabal new-run mary-tests -- -i -p Mary

test-shonkier:
	cabal new-run mary-tests -- -i -p '$$2 == "Shonkier"'

test-js:
	cabal new-run mary-tests -- -i -p ShonkierJS

TAGS:
	hasktags --etags .

bash-completion:
# Use as follows: source <(make bash-completion)
	mary --bash-completion-script `which mary`
