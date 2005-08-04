default: build test doc

build: $(patsubst %.hs,%.hi,$(SRCS)) libTimeLib.a

test: build
	cd test && make

cleantest:
	cd test && make clean

SRCS = Data/Fixed.hs \
	Data/Time/Clock/Scale.hs \
	Data/Time/Clock/UTC.hs \
	Data/Time/Clock/POSIX.hs \
	Data/Time/Clock/Current.hs \
	Data/Time/Clock.hs \
	Data/Time/TAI.hs \
	Data/Time/Calendar/Private.hs \
	Data/Time/Calendar/Days.hs \
	Data/Time/Calendar/YearDay.hs \
	Data/Time/Calendar/Gregorian.hs \
	Data/Time/Calendar/ISOWeekDay.hs \
	Data/Time/Calendar/Timezone.hs \
	Data/Time/Calendar/TimeOfDay.hs \
	Data/Time/Calendar/Calendar.hs \
	Data/Time/Calendar/Format.hs \
	Data/Time/Calendar.hs

timestuff.o: timestuff.c timestuff.h
	gcc -o $@ -c $<

libTimeLib.a: $(patsubst %.hs,%.o,$(SRCS)) timestuff.o
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

# TestTime.o TestFormat.o CurrentTime.o ShowDST.o TimeZone.o: $(patsubst %.hs,%.hi,$(SRCS))

TestFixed.o: Data/Fixed.hi

# DO NOT DELETE: Beginning of Haskell dependencies
Data/Time/Calendar/Days.o : Data/Time/Calendar/Days.hs
Data/Fixed.o : Data/Fixed.hs
Data/Time/Clock/Scale.o : Data/Time/Clock/Scale.hs
Data/Time/Clock/Scale.o : Data/Fixed.hi
Data/Time/Clock/UTC.o : Data/Time/Clock/UTC.hs
Data/Time/Clock/UTC.o : Data/Fixed.hi
Data/Time/Clock/UTC.o : Data/Time/Clock/Scale.hi
Data/Time/Clock/UTC.o : Data/Time/Calendar/Days.hi
Data/Time/Clock/POSIX.o : Data/Time/Clock/POSIX.hs
Data/Time/Clock/POSIX.o : Data/Time/Clock/UTC.hi
Data/Time/Clock/Current.o : Data/Time/Clock/Current.hs
Data/Time/Clock/Current.o : Data/Time/Clock/UTC.hi
Data/Time/Clock.o : Data/Time/Clock.hs
Data/Time/Clock.o : Data/Time/Clock/Current.hi
Data/Time/Clock.o : Data/Time/Clock/UTC.hi
Data/Time/Clock.o : Data/Time/Clock/Scale.hi
Data/Time/TAI.o : Data/Time/TAI.hs
Data/Time/TAI.o : Data/Time/Clock.hi
Data/Time/TAI.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Private.o : Data/Time/Calendar/Private.hs
Data/Time/Calendar/Private.o : Data/Fixed.hi
Data/Time/Calendar/YearDay.o : Data/Time/Calendar/YearDay.hs
Data/Time/Calendar/YearDay.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/YearDay.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Gregorian.hs
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Gregorian.o : Data/Time/Calendar/YearDay.hi
Data/Time/Calendar/ISOWeekDay.o : Data/Time/Calendar/ISOWeekDay.hs
Data/Time/Calendar/ISOWeekDay.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/ISOWeekDay.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/ISOWeekDay.o : Data/Time/Calendar/YearDay.hi
Data/Time/Calendar/Timezone.o : Data/Time/Calendar/Timezone.hs
Data/Time/Calendar/Timezone.o : Data/Time/Clock/POSIX.hi
Data/Time/Calendar/Timezone.o : Data/Time/Clock.hi
Data/Time/Calendar/Timezone.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/TimeOfDay.o : Data/Time/Calendar/TimeOfDay.hs
Data/Time/Calendar/TimeOfDay.o : Data/Fixed.hi
Data/Time/Calendar/TimeOfDay.o : Data/Time/Clock.hi
Data/Time/Calendar/TimeOfDay.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/TimeOfDay.o : Data/Time/Calendar/Timezone.hi
Data/Time/Calendar/Calendar.o : Data/Time/Calendar/Calendar.hs
Data/Time/Calendar/Calendar.o : Data/Time/Clock.hi
Data/Time/Calendar/Calendar.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Calendar.o : Data/Time/Calendar/Gregorian.hi
Data/Time/Calendar/Calendar.o : Data/Time/Calendar/Timezone.hi
Data/Time/Calendar/Calendar.o : Data/Time/Calendar/TimeOfDay.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Format.hs
Data/Time/Calendar/Format.o : Data/Time/Clock/POSIX.hi
Data/Time/Calendar/Format.o : Data/Time/Clock.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Private.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Timezone.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/TimeOfDay.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Calendar.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Days.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/YearDay.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/Gregorian.hi
Data/Time/Calendar/Format.o : Data/Time/Calendar/ISOWeekDay.hi
Data/Time/Calendar.o : Data/Time/Calendar.hs
Data/Time/Calendar.o : Data/Time/Calendar/Format.hi
Data/Time/Calendar.o : Data/Time/Calendar/Calendar.hi
Data/Time/Calendar.o : Data/Time/Calendar/TimeOfDay.hi
Data/Time/Calendar.o : Data/Time/Calendar/Timezone.hi
Data/Time/Calendar.o : Data/Time/Calendar/ISOWeekDay.hi
Data/Time/Calendar.o : Data/Time/Calendar/Gregorian.hi
Data/Time/Calendar.o : Data/Time/Calendar/YearDay.hi
Data/Time/Calendar.o : Data/Time/Calendar/Days.hi
# DO NOT DELETE: End of Haskell dependencies
