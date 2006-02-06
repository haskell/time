default: build test

cabal-configure:
	runghc Setup.hs configure

cabal-build: cabal-configure
	runghc Setup.hs build

cabal-install: cabal-build
	sudo runghc Setup.hs install

PACKAGENAME = fixed
LIBFILE = libHS$(PACKAGENAME).a

build: $(patsubst %.hs,%.hi,$(SRCS)) $(LIBFILE)

test: build
	cd test && make

cleantest:
	cd test && make clean

SRCS = Data/Fixed.hs

$(LIBFILE): $(patsubst %.hs,%.o,$(SRCS))
	rm -f $@
	ar cru $@ $^
	ranlib $@

cleanbuild:
	rm -rf *.a *.o *.hi $(patsubst %.hs,%.o,$(SRCS)) $(patsubst %.hs,%.hi,$(SRCS)) Makefile.bak

clean: cleantest cleanbuild

sources:
	echo $(SRCS) > $@

%.out: %
	./$< > $@

%.run: %
	./$<

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< -o $@

FORCE:

.SECONDARY:

.PHONY: default build test clean

depend: $(SRCS)
	ghc -M $^

# DO NOT DELETE: Beginning of Haskell dependencies
Data/Fixed.o : Data/Fixed.hs
# DO NOT DELETE: End of Haskell dependencies
