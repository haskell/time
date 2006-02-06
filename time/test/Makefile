default: CurrentTime.run ShowDST.run test

TestMonthDay: TestMonthDay.o ../libHStime.a
	ghc -package fixed $^ -o $@

ConvertBack: ConvertBack.o ../libHStime.a
	ghc -package fixed $^ -o $@

TestCalendars: TestCalendars.o ../libHStime.a
	ghc -package fixed $^ -o $@

TestTime: TestTime.o ../libHStime.a
	ghc -package fixed $^ -o $@

LongWeekYears: LongWeekYears.o ../libHStime.a
	ghc -package fixed $^ -o $@

ClipDates: ClipDates.o ../libHStime.a
	ghc -package fixed $^ -o $@

AddDays: AddDays.o ../libHStime.a
	ghc -package fixed $^ -o $@

TestFormat: TestFormat.o TestFormatStuff.o ../libHStime.a
	ghc -package fixed $^ -o $@

TestFormatStuff.o: TestFormatStuff.c TestFormatStuff.h
	gcc -o $@ -c $<

TestParseDAT: TestParseDAT.o ../libHStime.a
	ghc -package fixed $^ -o $@

TestEaster: TestEaster.o ../libHStime.a
	ghc -package fixed $^ -o $@

CurrentTime: CurrentTime.o ../libHStime.a
	ghc -package fixed $^ -o $@

ShowDST: ShowDST.o ../libHStime.a
	ghc -package fixed $^ -o $@

TimeZone: TimeZone.o ../libHStime.a
	ghc -package fixed $^ -o $@

TimeZone.ref: FORCE
	date +%z > $@

test:	\
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
	rm -rf TestMonthDay ConvertBack TestCalendars TestTime LongWeekYears ClipDates	\
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
