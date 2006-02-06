default: build test doc

cabal-configure:
	runghc Setup.hs configure

cabal-build: cabal-configure
	runghc Setup.hs build

cabal-install: cabal-build
	sudo runghc Setup.hs install

PACKAGENAME = time
LIBFILE = libHS$(PACKAGENAME).a

build: $(patsubst %.hs,%.hi,$(SRCS)) $(LIBFILE)

test: build
	cd test && make

cleantest:
	cd test && make clean

SRCS = \
	Data/Time/Calendar/Private.hs \
	Data/Time/Calendar/Days.hs \
	Data/Time/Calendar/OrdinalDate.hs \
	Data/Time/Calendar/MonthDay.hs \
	Data/Time/Calendar/Gregorian.hs \
	Data/Time/Calendar/WeekDate.hs \
	Data/Time/Calendar/JulianYearDay.hs \
	Data/Time/Calendar/Julian.hs \
	Data/Time/Calendar/Easter.hs \
	Data/Time/Calendar.hs \
	Data/Time/Clock/Scale.hs \
	Data/Time/Clock/UTC.hs \
	Data/Time/Clock/CTimeval.hs \
	Data/Time/Clock/POSIX.hs \
	Data/Time/Clock/UTCDiff.hs \
	Data/Time/Clock/TAI.hs \
	Data/Time/Clock.hs \
	Data/Time/LocalTime/TimeZone.hs \
	Data/Time/LocalTime/TimeOfDay.hs \
	Data/Time/LocalTime/LocalTime.hs \
	Data/Time/LocalTime/Format.hs \
	Data/Time/LocalTime.hs \
	Data/Time.hs

timestuff.o: timestuff.c timestuff.h
	gcc -o $@ -c $<

$(LIBFILE): $(patsubst %.hs,%.o,$(SRCS)) timestuff.o
	rm -f $@
	ar cru $@ $^
	ranlib $@

cleanbuild:
	rm -rf *.a *.o *.hi $(patsubst %.hs,%.o,$(SRCS)) $(patsubst %.hs,%.hi,$(SRCS)) Makefile.bak

cleandoc:
	rm -rf doc haddock

clean: cleandoc cleantest cleanbuild

doc: haddock/index.html

haddock/index.html: $(SRCS)
	mkdir -p haddock
	haddock -h -o haddock $^

sources:
	echo $(SRCS) > $@

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

FORCE:

.SECONDARY:

.PHONY: default build test doc clean

depend: $(SRCS)
	ghc -M $^

# DO NOT DELETE: Beginning of Haskell dependencies
Data/Time/Clock/CTimeval.o : Data/Time/Clock/CTimeval.hs
Data/Time/Clock/Scale.o : Data/Time/Clock/Scale.hs
Data/Time/Calendar/Private.o : Data/Time/Calendar/Private.hs
Data/Time/Calendar/MonthDay.o : Data/Time/Calendar/MonthDay.hs
Data/Time/Calendar/MonthDay.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/Days.o : Data/Time/Calendar/Days.hs
Data/Time/Calendar/OrdinalDate.o : Data/Time/Calendar/OrdinalDate.hs
Data/Time/Calendar/OrdinalDate.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/OrdinalDate.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Gregorian.hs
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/OrdinalDate.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/MonthDay.hi
Data/Time/Calendar/WeekDate.o : Data/Time/Calendar/WeekDate.hs
Data/Time/Calendar/WeekDate.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/WeekDate.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/WeekDate.o : Data/Time/Calendar/OrdinalDate.hi
Data/Time/Calendar/JulianYearDay.o : Data/Time/Calendar/JulianYearDay.hs
Data/Time/Calendar/JulianYearDay.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/JulianYearDay.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Julian.o : Data/Time/Calendar/Julian.hs
Data/Time/Calendar/Julian.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/Julian.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Julian.o : Data/Time/Calendar/JulianYearDay.hi
Data/Time/Calendar/Julian.o : Data/Time/Calendar/MonthDay.hi
Data/Time/Calendar.o : Data/Time/Calendar.hs
Data/Time/Calendar.o : Data/Time/Calendar/Gregorian.hi
Data/Time/Calendar.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Easter.o : Data/Time/Calendar/Easter.hs
Data/Time/Calendar/Easter.o : Data/Time/Calendar/Julian.hi
Data/Time/Calendar/Easter.o : Data/Time/Calendar.hi
Data/Time/Clock/UTC.o : Data/Time/Clock/UTC.hs
Data/Time/Clock/UTC.o : Data/Time/Clock/Scale.hi
Data/Time/Clock/UTC.o : Data/Time/Calendar/Days.hi
Data/Time/Clock/POSIX.o : Data/Time/Clock/POSIX.hs
Data/Time/Clock/POSIX.o : Data/Time/Calendar/Days.hi
Data/Time/Clock/POSIX.o : Data/Time/Clock/UTC.hi
Data/Time/Clock/POSIX.o : Data/Time/Clock/CTimeval.hi
Data/Time/Clock/UTCDiff.o : Data/Time/Clock/UTCDiff.hs
Data/Time/Clock/UTCDiff.o : Data/Time/Clock/UTC.hi
Data/Time/Clock/UTCDiff.o : Data/Time/Clock/POSIX.hi
Data/Time/Clock.o : Data/Time/Clock.hs
Data/Time/Clock.o : Data/Time/Clock/POSIX.hi
Data/Time/Clock.o : Data/Time/Clock/UTC.hi
Data/Time/Clock.o : Data/Time/Clock/UTCDiff.hi
Data/Time/Clock.o : Data/Time/Clock/Scale.hi
Data/Time/LocalTime/TimeZone.o : Data/Time/LocalTime/TimeZone.hs
Data/Time/LocalTime/TimeZone.o : Data/Time/Clock/POSIX.hi
Data/Time/LocalTime/TimeZone.o : Data/Time/Clock.hi
Data/Time/LocalTime/TimeZone.o : Data/Time/Calendar/Private.hi
Data/Time/LocalTime/TimeOfDay.o : Data/Time/LocalTime/TimeOfDay.hs
Data/Time/LocalTime/TimeOfDay.o : Data/Time/Clock.hi
Data/Time/LocalTime/TimeOfDay.o : Data/Time/Calendar/Private.hi
Data/Time/LocalTime/TimeOfDay.o : Data/Time/LocalTime/TimeZone.hi
Data/Time/LocalTime/LocalTime.o : Data/Time/LocalTime/LocalTime.hs
Data/Time/LocalTime/LocalTime.o : Data/Time/Clock.hi
Data/Time/LocalTime/LocalTime.o : Data/Time/Calendar.hi
Data/Time/LocalTime/LocalTime.o : Data/Time/LocalTime/TimeZone.hi
Data/Time/LocalTime/LocalTime.o : Data/Time/LocalTime/TimeOfDay.hi
Data/Time/LocalTime/Format.o : Data/Time/LocalTime/Format.hs
Data/Time/LocalTime/Format.o : Data/Time/Clock/POSIX.hi
Data/Time/LocalTime/Format.o : Data/Time/Clock.hi
Data/Time/LocalTime/Format.o : Data/Time/Calendar/Private.hi
Data/Time/LocalTime/Format.o : Data/Time/Calendar.hi
Data/Time/LocalTime/Format.o : Data/Time/Calendar/OrdinalDate.hi
Data/Time/LocalTime/Format.o : Data/Time/Calendar/WeekDate.hi
Data/Time/LocalTime/Format.o : Data/Time/LocalTime/TimeZone.hi
Data/Time/LocalTime/Format.o : Data/Time/LocalTime/TimeOfDay.hi
Data/Time/LocalTime/Format.o : Data/Time/LocalTime/LocalTime.hi
Data/Time/LocalTime.o : Data/Time/LocalTime.hs
Data/Time/LocalTime.o : Data/Time/LocalTime/Format.hi
Data/Time/LocalTime.o : Data/Time/LocalTime/LocalTime.hi
Data/Time/LocalTime.o : Data/Time/LocalTime/TimeOfDay.hi
Data/Time/LocalTime.o : Data/Time/LocalTime/TimeZone.hi
Data/Time.o : Data/Time.hs
Data/Time.o : Data/Time/LocalTime.hi
Data/Time.o : Data/Time/Clock.hi
Data/Time.o : Data/Time/Calendar.hi
Data/Time/Clock/TAI.o : Data/Time/Clock/TAI.hs
Data/Time/Clock/TAI.o : Data/Time/Clock.hi
Data/Time/Clock/TAI.o : Data/Time/Calendar/Days.hi
Data/Time/Clock/TAI.o : Data/Time/LocalTime.hi
# DO NOT DELETE: End of Haskell dependencies
