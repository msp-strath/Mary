SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')

.PHONY: clean install-hasktags TAGS bash-completion

# From https://stackoverflow.com/questions/18649427/using-a-makefile-with-cabal
# If 'cabal install' fails in building or installing, the
# timestamp on the build dir 'dist' may still be updated.  So,
# we set the timestamp on the build dir to a long time in the past
# with 'touch --date "@0"' in case cabal fails.
CABAL_INSTALL = \
  cabal new-install exe:mary --overwrite-policy=always \
  || { touch --date "@0" dist ; \
       exit 42 ; }

install: $(SOURCE)
	$(CABAL_INSTALL)
	cp ~/.cabal/bin/mary .

clean:
	rm -rf dist TAGS

install-hasktags:
	cabal update
	cabal install hasktags

.PHONY: test test-mary test-js test-shonkier
test:
	cabal new-run mary-tests -- -i
#	runhaskell -itest test/Test/Main.hs -i

test-mary:
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
