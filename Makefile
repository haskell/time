default: TestFixed.diff CurrentTime.run TestTime.diff doc

SRCS = Data/Fixed.hs System/Time/Clock.hs System/Time/TAI.hs System/Time/Calendar.hs

TestFixed: TestFixed.o Data/Fixed.o
	ghc $^ -o $@

TestTime: TestTime.o libTimeLib.a
	ghc $^ -o $@

CurrentTime: CurrentTime.o libTimeLib.a
	ghc $^ -o $@

libTimeLib.a: $(patsubst %.hs,%.o,$(SRCS))
	rm -f $@
	ar cru $@ $^
	ranlib $@

clean:
	rm -rf TestTime TestFixed doc haddock *.out *.o *.hi $(patsubst %.hs,%.o,$(SRCS)) $(patsubst %.hs,%.hi,$(SRCS)) Makefile.bak

doc: haddock/index.html

haddock/index.html: $(SRCS)
	mkdir -p haddock
	haddock -h -o haddock $^

%.diff: %.ref %.out
	diff -u $^

%.out: %
	./$< > $@

%.run: %
	./$<

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< -o $@

.SECONDARY:

depend: TestFixed.hs CurrentTime.hs TestTime.hs $(SRCS)
	ghc -M $^

# DO NOT DELETE: Beginning of Haskell dependencies
TestFixed.o : TestFixed.hs
TestFixed.o : ./Data/Fixed.hi
CurrentTime.o : CurrentTime.hs
CurrentTime.o : ./System/Time/Calendar.hi
CurrentTime.o : ./System/Time/TAI.hi
CurrentTime.o : ./System/Time/Clock.hi
TestTime.o : TestTime.hs
TestTime.o : ./System/Time/Calendar.hi
TestTime.o : ./System/Time/TAI.hi
TestTime.o : ./System/Time/Clock.hi
Data/Fixed.o : Data/Fixed.hs
System/Time/Clock.o : System/Time/Clock.hs
System/Time/Clock.o : Data/Fixed.hi
System/Time/TAI.o : System/Time/TAI.hs
System/Time/TAI.o : Data/Fixed.hi
System/Time/TAI.o : System/Time/Clock.hi
System/Time/Calendar.o : System/Time/Calendar.hs
System/Time/Calendar.o : Data/Fixed.hi
System/Time/Calendar.o : System/Time/Clock.hi
# DO NOT DELETE: End of Haskell dependencies
