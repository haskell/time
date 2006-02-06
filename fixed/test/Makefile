default: test

TestFixed: TestFixed.o ../Data/Fixed.o
	ghc $^ -o $@

test:	\
	TestFixed.diff

clean:
	rm -rf TestFixed *.out *.o *.hi Makefile.bak

%.diff: %.ref %.out
	diff -u $^

%.diff0: %.out
	echo -n | diff -u - $^

%.out: %
	./$< > $@

%.run: %
	./$<

%.hi: %.o
	@:

%.o: %.hs
	ghc -i.. -c $< -o $@

%.o: %.lhs
	ghc -i.. -c $< -o $@

FORCE:

.SECONDARY:

# TestTime.o TestFormat.o CurrentTime.o ShowDST.o TimeZone.o: $(patsubst %.hs,%.hi,$(SRCS))

TestFixed.o: ../Data/Fixed.hi
