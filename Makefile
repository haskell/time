default: TestTime.run

# SRCS = System/Time/Clock.hs System/Time/TAI.hs System/Time/Calendar.hs
SRCS = System/Time/Clock.hs System/Time/TAI.hs

TestTime: TestTime.o $(patsubst %.hs,%.o,$(SRCS))
	ghc $^ -o $@


clean:
	rm -f TestTime *.o *.hi $(patsubst %.hs,%.o,$(SRCS)) $(patsubst %.hs,%.hi,$(SRCS)) Makefile.bak


doc: $(SRCS)
	mkdir -p $@
	haddock -h -o $@ $^


%.run: %
	./$<

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< -o $@

depend: TestTime.hs $(SRCS)
	ghc -M $^

# DO NOT DELETE: Beginning of Haskell dependencies
TestTime.o : TestTime.hs
TestTime.o : ./System/Time/TAI.hi
TestTime.o : ./System/Time/Clock.hi
System/Time/Clock.o : System/Time/Clock.hs
System/Time/TAI.o : System/Time/TAI.hs
System/Time/TAI.o : System/Time/Clock.hi
# DO NOT DELETE: End of Haskell dependencies
