default: CurrentTime.run TestTime.diff

SRCS = System/Time/Clock.hs System/Time/TAI.hs System/Time/Calendar.hs

TestTime: TestTime.o $(patsubst %.hs,%.o,$(SRCS))
	ghc $^ -o $@

CurrentTime: CurrentTime.o $(patsubst %.hs,%.o,$(SRCS))
	ghc $^ -o $@


clean:
	rm -f TestTime *.o *.hi $(patsubst %.hs,%.o,$(SRCS)) $(patsubst %.hs,%.hi,$(SRCS)) Makefile.bak


doc: $(SRCS)
	mkdir -p $@
	haddock -h -o $@ $^

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

depend: CurrentTime.hs TestTime.hs $(SRCS)
	ghc -M $^

# DO NOT DELETE: Beginning of Haskell dependencies
CurrentTime.o : CurrentTime.hs
CurrentTime.o : ./System/Time/Calendar.hi
CurrentTime.o : ./System/Time/TAI.hi
CurrentTime.o : ./System/Time/Clock.hi
TestTime.o : TestTime.hs
TestTime.o : ./System/Time/Calendar.hi
TestTime.o : ./System/Time/TAI.hi
TestTime.o : ./System/Time/Clock.hi
System/Time/Clock.o : System/Time/Clock.hs
System/Time/TAI.o : System/Time/TAI.hs
System/Time/TAI.o : System/Time/Clock.hi
System/Time/Calendar.o : System/Time/Calendar.hs
System/Time/Calendar.o : System/Time/Clock.hi
# DO NOT DELETE: End of Haskell dependencies
