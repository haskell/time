default: TestTime.run

#TestTime: TestTime.o System/Time/Clock.o System/Time/TAI.o System/Time/Calendar.o
TestTime: TestTime.o System/Time/Clock.o
	ghc $^ -o $@


clean:
	rm -f TestTime *.o *.hi System/Time/*.o System/Time/*.hi Makefile.bak


%.run: %
	./$<

%.hi: %.o
	@:

%.o: %.hs
	ghc -c $< -o $@

depend: TestTime.hs System/Time/Clock.hs System/Time/TAI.hs System/Time/Calendar.hs
	ghc -M $^
# DO NOT DELETE: Beginning of Haskell dependencies
TestTime.o : TestTime.hs
TestTime.o : ./System/Time/Clock.hi
System/Time/Clock.o : System/Time/Clock.hs
System/Time/TAI.o : System/Time/TAI.hs
System/Time/TAI.o : System/Time/Clock.hi
System/Time/Calendar.o : System/Time/Calendar.hs
# DO NOT DELETE: End of Haskell dependencies
