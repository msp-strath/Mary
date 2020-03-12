SOURCE = $(shell find src -name '*.lhs' -o -name '*.hs')

.PHONY: clean install-hasktags TAGS

# From https://stackoverflow.com/questions/18649427/using-a-makefile-with-cabal
# If 'cabal install' fails in building or installing, the
# timestamp on the build dir 'dist' may still be updated.  So,
# we set the timestamp on the build dir to a long time in the past
# with 'touch --date "@0"' in case cabal fails.
CABAL_INSTALL = \
  cabal install \
  || { touch --date "@0" dist ; \
       exit 42 ; }

install: $(SOURCE)
	$(CABAL_INSTALL)

clean:
	rm -rf dist TAGS

install-hasktags:
	cabal update
	cabal install hasktags

.PHONY: test
test:
	runhaskell -itest test/Test/Main.hs -i

TAGS:
	hasktags --etags .
