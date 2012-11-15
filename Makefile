default: install

# Building

clean:
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests

build: configure
	cabal build --ghc-options=-Werror

test: build
	cabal test --test-option=--hide-successes --test-option=--color

haddock: configure
	cabal haddock

install: build test haddock
	cabal install --user --enable-library-profiling --enable-executable-profiling

sdist: clean configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock install test sdist
