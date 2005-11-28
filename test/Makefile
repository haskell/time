default: CurrentTime.run ShowDST.run test

TestFixed: TestFixed.o ../Data/Fixed.o
	ghc $^ -o $@

TestMonthDay: TestMonthDay.o ../libTimeLib.a
	ghc $^ -o $@

ConvertBack: ConvertBack.o ../libTimeLib.a
	ghc $^ -o $@

TestCalendars: TestCalendars.o ../libTimeLib.a
	ghc $^ -o $@

TestTime: TestTime.o ../libTimeLib.a
	ghc $^ -o $@

LongWeekYears: LongWeekYears.o ../libTimeLib.a
	ghc $^ -o $@

ClipDates: ClipDates.o ../libTimeLib.a
	ghc $^ -o $@

AddDays: AddDays.o ../libTimeLib.a
	ghc $^ -o $@

TestFormat: TestFormat.o TestFormatStuff.o ../libTimeLib.a
	ghc $^ -o $@

TestFormatStuff.o: TestFormatStuff.c TestFormatStuff.h
	gcc -o $@ -c $<

TestParseDAT: TestParseDAT.o ../libTimeLib.a
	ghc $^ -o $@

TestEaster: TestEaster.o ../libTimeLib.a
	ghc $^ -o $@

CurrentTime: CurrentTime.o ../libTimeLib.a
	ghc $^ -o $@

ShowDST: ShowDST.o ../libTimeLib.a
	ghc $^ -o $@

TimeZone: TimeZone.o ../libTimeLib.a
	ghc $^ -o $@

TimeZone.ref: FORCE
	date +%z > $@

test:	\
	TestFixed.diff	\
	TestMonthDay.diff	\
	ConvertBack.diff0	\
	TestCalendars.diff	\
	TestTime.diff	\
	LongWeekYears.diff	\
	ClipDates.diff	\
	AddDays.diff	\
	TimeZone.diff	\
	TestFormat.diff0	\
	TestParseDAT.diff	\
	TestEaster.diff	\
	UseCases.o

clean:
	rm -rf TestFixed TestMonthDay ConvertBack TestCalendars TestTime LongWeekYears ClipDates	\
	AddDays TestFormat TestParseDAT TestEaster CurrentTime ShowDST TimeZone TimeZone.ref *.out *.o *.hi Makefile.bak

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
