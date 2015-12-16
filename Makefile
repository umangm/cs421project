GMAKE=make

all: tr cpstr

tr:
	$(GMAKE) -C tailrec/
	ln -sf tailrec/tailRecPicoMLInt ./tailRecPicoML

cpstr:
	$(GMAKE) -C tailrecCPS/
	ln -sf tailrecCPS/tailRecPicoMLInt ./tailRecPicoMLCPS

clean:
	$(GMAKE) -C tailrec/ clean
	$(GMAKE) -C tailrecCPS/ clean
	rm -f tailRecPicoML
	rm -f tailRecPicoMLCPS
