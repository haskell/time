default: build test doc

build: $(patsubst %.hs,%.hi,$(SRCS)) libTimeLib.a

test: build
	cd test && make

cleantest:
	cd test && make clean

SRCS = Data/Fixed.hs \
	System/Time/Clock/Scale.hs \
	System/Time/Clock/UTC.hs \
	System/Time/Clock/POSIX.hs \
	System/Time/Clock/Current.hs \
	System/Time/Clock.hs \
	System/Time/TAI.hs \
	System/Time/Calendar/Private.hs \
	System/Time/Calendar/Days.hs \
	System/Time/Calendar/YearDay.hs \
	System/Time/Calendar/Gregorian.hs \
	System/Time/Calendar/ISOWeekDay.hs \
	System/Time/Calendar/Timezone.hs \
	System/Time/Calendar/TimeOfDay.hs \
	System/Time/Calendar/Calendar.hs \
	System/Time/Calendar/Format.hs \
	System/Time/Calendar.hs

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
Data/Fixed.o : Data/Fixed.hs
System/Time/Clock/Scale.o : System/Time/Clock/Scale.hs
System/Time/Clock/Scale.o : Data/Fixed.hi
System/Time/Clock/UTC.o : System/Time/Clock/UTC.hs
System/Time/Clock/UTC.o : Data/Fixed.hi
System/Time/Clock/UTC.o : System/Time/Clock/Scale.hi
System/Time/Clock/POSIX.o : System/Time/Clock/POSIX.hs
System/Time/Clock/POSIX.o : System/Time/Clock/UTC.hi
System/Time/Clock/Current.o : System/Time/Clock/Current.hs
System/Time/Clock/Current.o : System/Time/Clock/UTC.hi
System/Time/Clock.o : System/Time/Clock.hs
System/Time/Clock.o : System/Time/Clock/Current.hi
System/Time/Clock.o : System/Time/Clock/UTC.hi
System/Time/Clock.o : System/Time/Clock/Scale.hi
System/Time/TAI.o : System/Time/TAI.hs
System/Time/TAI.o : System/Time/Clock.hi
System/Time/Calendar/Days.o : System/Time/Calendar/Days.hs
System/Time/Calendar/Days.o : System/Time/Clock.hi
System/Time/Calendar/Private.o : System/Time/Calendar/Private.hs
System/Time/Calendar/Private.o : Data/Fixed.hi
System/Time/Calendar/YearDay.o : System/Time/Calendar/YearDay.hs
System/Time/Calendar/YearDay.o : System/Time/Clock.hi
System/Time/Calendar/YearDay.o : System/Time/Calendar/Private.hi
System/Time/Calendar/YearDay.o : System/Time/Calendar/Days.hi
System/Time/Calendar/Gregorian.o : System/Time/Calendar/Gregorian.hs
System/Time/Calendar/Gregorian.o : System/Time/Calendar/Private.hi
System/Time/Calendar/Gregorian.o : System/Time/Calendar/Days.hi
System/Time/Calendar/Gregorian.o : System/Time/Calendar/YearDay.hi
System/Time/Calendar/ISOWeekDay.o : System/Time/Calendar/ISOWeekDay.hs
System/Time/Calendar/ISOWeekDay.o : System/Time/Calendar/Private.hi
System/Time/Calendar/ISOWeekDay.o : System/Time/Calendar/Days.hi
System/Time/Calendar/ISOWeekDay.o : System/Time/Calendar/YearDay.hi
System/Time/Calendar/Timezone.o : System/Time/Calendar/Timezone.hs
System/Time/Calendar/Timezone.o : System/Time/Clock/POSIX.hi
System/Time/Calendar/Timezone.o : System/Time/Clock.hi
System/Time/Calendar/Timezone.o : System/Time/Calendar/Private.hi
System/Time/Calendar/TimeOfDay.o : System/Time/Calendar/TimeOfDay.hs
System/Time/Calendar/TimeOfDay.o : Data/Fixed.hi
System/Time/Calendar/TimeOfDay.o : System/Time/Clock.hi
System/Time/Calendar/TimeOfDay.o : System/Time/Calendar/Private.hi
System/Time/Calendar/TimeOfDay.o : System/Time/Calendar/Timezone.hi
System/Time/Calendar/Calendar.o : System/Time/Calendar/Calendar.hs
System/Time/Calendar/Calendar.o : System/Time/Clock.hi
System/Time/Calendar/Calendar.o : System/Time/Calendar/Days.hi
System/Time/Calendar/Calendar.o : System/Time/Calendar/Timezone.hi
System/Time/Calendar/Calendar.o : System/Time/Calendar/TimeOfDay.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Format.hs
System/Time/Calendar/Format.o : System/Time/Clock/POSIX.hi
System/Time/Calendar/Format.o : System/Time/Clock.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Private.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Timezone.hi
System/Time/Calendar/Format.o : System/Time/Calendar/TimeOfDay.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Calendar.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Days.hi
System/Time/Calendar/Format.o : System/Time/Calendar/YearDay.hi
System/Time/Calendar/Format.o : System/Time/Calendar/Gregorian.hi
System/Time/Calendar/Format.o : System/Time/Calendar/ISOWeekDay.hi
System/Time/Calendar.o : System/Time/Calendar.hs
System/Time/Calendar.o : System/Time/Calendar/Format.hi
System/Time/Calendar.o : System/Time/Calendar/Calendar.hi
System/Time/Calendar.o : System/Time/Calendar/TimeOfDay.hi
System/Time/Calendar.o : System/Time/Calendar/Timezone.hi
System/Time/Calendar.o : System/Time/Calendar/ISOWeekDay.hi
System/Time/Calendar.o : System/Time/Calendar/Gregorian.hi
System/Time/Calendar.o : System/Time/Calendar/YearDay.hi
System/Time/Calendar.o : System/Time/Calendar/Days.hi
System/Time/Calendar.o : System/Time/Clock.hi
System/Time/Calendar.o : Data/Fixed.hi
# DO NOT DELETE: End of Haskell dependencies
