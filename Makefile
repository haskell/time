default: clean test install test-sdist

# Building

clean:
	rm -rf test-sdist
	cabal clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests

build: configure
	cabal build --ghc-options=-Werror

test: configure
	cabal test --ghc-options=-Werror --test-option=--hide-successes --test-option=--color

haddock: configure
	cabal haddock

copy: build test haddock
	cabal copy

install:
	cabal install --user --ghc-options=-Werror --enable-library-profiling --enable-executable-profiling

sdist: clean configure
	cabal sdist

test-sdist: sdist
	mkdir -p test-sdist
	tar -C test-sdist -z -x -f dist/time-1.5.1.tar.gz
	cp Makefile test-sdist/time-1.5.1/
	cd test-sdist/time-1.5.1 && make test

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock copy install test sdist test-sdist
