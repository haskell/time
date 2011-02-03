GHC = ghc
GHCFLAGS = -package time

default:
	make CurrentTime.run ShowDST.run test

TestMonthDay: TestMonthDay.o
	$(GHC) $(GHCFLAGS) $^ -o $@

ConvertBack: ConvertBack.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TestCalendars: TestCalendars.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TestTime: TestTime.o
	$(GHC) $(GHCFLAGS) $^ -o $@

LongWeekYears: LongWeekYears.o
	$(GHC) $(GHCFLAGS) $^ -o $@

ClipDates: ClipDates.o
	$(GHC) $(GHCFLAGS) $^ -o $@

AddDays: AddDays.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TestFormat: TestFormat.o TestFormatStuff.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TestFormatStuff.o: TestFormatStuff.c TestFormatStuff.h
	gcc -o $@ -c $<

TestParseDAT: TestParseDAT.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TestEaster: TestEaster.o
	$(GHC) $(GHCFLAGS) $^ -o $@

CurrentTime: CurrentTime.o
	$(GHC) $(GHCFLAGS) $^ -o $@

ShowDST: ShowDST.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TimeZone: TimeZone.o
	$(GHC) $(GHCFLAGS) $^ -o $@

TimeZone.ref: FORCE
	date +%z > $@

TestParseTime: TestParseTime.o
	$(GHC) $(GHCFLAGS) -package QuickCheck $^ -o $@

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
	TestParseTime.run \
	UseCases.o
	@echo "Success!"

clean:
	rm -rf TestMonthDay ConvertBack TestCalendars TestTime LongWeekYears ClipDates	\
	AddDays TestFormat TestParseDAT TestEaster CurrentTime ShowDST TimeZone TimeZone.ref TestParseTime \
        *.out *.run *.o *.hi Makefile.bak

%.diff: %.ref %.out
	diff -u $^

%.diff0: %.out
	diff -u /dev/null $^

%.out: %
	./$< > $@

%.run: %
	./$<
	touch $@

%.hi: %.o
	@:

%.o: %.hs
	$(GHC) $(GHCFLAGS) -c $< -o $@

%.o: %.lhs
	$(GHC) $(GHCFLAGS) -c $< -o $@

FORCE:

.SECONDARY:

