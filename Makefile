default: time-build

fixed-%:
	cd fixed && make $*

time-%: fixed-cabal-install
	cd time && make $*

sources: fixed/Makefile time/Makefile Makefile
	cd fixed && make sources
	cd time && make sources
	echo > sources
	for f in `cat fixed/sources`; do echo fixed/$$f >> $@; done
	for f in `cat time/sources`; do echo time/$$f >> $@; done

# unified Haddock
doc: sources
	mkdir -p haddock
	haddock -h -o haddock `cat sources`

clean:
	rm -f sources haddock
	cd time && make clean
	cd fixed && make clean
