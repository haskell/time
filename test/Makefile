GHC = ghc
GHCFLAGS = 
LIBS = ../../dist/build/libHStime-1.0.a

default: CurrentTime.run ShowDST.run test

TestMonthDay: TestMonthDay.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

ConvertBack: ConvertBack.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TestCalendars: TestCalendars.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TestTime: TestTime.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

LongWeekYears: LongWeekYears.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

ClipDates: ClipDates.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

AddDays: AddDays.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TestFormat: TestFormat.o TestFormatStuff.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TestFormatStuff.o: TestFormatStuff.c TestFormatStuff.h
	gcc -o $@ -c $<

TestParseDAT: TestParseDAT.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TestEaster: TestEaster.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

CurrentTime: CurrentTime.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

ShowDST: ShowDST.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TimeZone: TimeZone.o $(LIBS)
	$(GHC) $(GHCFLAGS) $^ -o $@

TimeZone.ref: FORCE
	date +%z > $@

TestParseTime: TestParseTime.o $(LIBS)
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

clean:
	rm -rf TestMonthDay ConvertBack TestCalendars TestTime LongWeekYears ClipDates	\
	AddDays TestFormat TestParseDAT TestEaster CurrentTime ShowDST TimeZone TimeZone.ref TestParseTime \
        *.out *.o *.hi Makefile.bak

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
