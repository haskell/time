default: CurrentTime.run ShowDST.run test

TestFixed: TestFixed.o ../Data/Fixed.o
	ghc $^ -o $@

ConvertBack: ConvertBack.o ../libTimeLib.a
	ghc $^ -o $@

TestTime: TestTime.o ../libTimeLib.a
	ghc $^ -o $@

TestFormat: TestFormat.o TestFormatStuff.o ../libTimeLib.a
	ghc $^ -o $@

TestFormatStuff.o: TestFormatStuff.c TestFormatStuff.h
	gcc -o $@ -c $<

CurrentTime: CurrentTime.o ../libTimeLib.a
	ghc $^ -o $@

ShowDST: ShowDST.o ../libTimeLib.a
	ghc $^ -o $@

TimeZone: TimeZone.o ../libTimeLib.a
	ghc $^ -o $@

TimeZone.ref: FORCE
	date +%z > $@

test: TestFixed.diff ConvertBack.diff0 TestTime.diff TimeZone.diff TestFormat.diff0

clean:
	rm -rf ConvertBack TimeZone TimeZone.ref CurrentTime TestTime TestFixed ShowDST TestFormat *.out *.o *.hi Makefile.bak

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

FORCE:

.SECONDARY:

# TestTime.o TestFormat.o CurrentTime.o ShowDST.o TimeZone.o: $(patsubst %.hs,%.hi,$(SRCS))

TestFixed.o: ../Data/Fixed.hi
