.PHONY: all clean

OUTFILES=morans tojs samples.js brain.js digit.js

all: $(OUTFILES)

morans: morans.hs; ghc -O2 $<
tojs: tojs.hs; ghc -O2 $<
samples.js: morans; ./morans samplesjs > $@
brain.js: 9202 tojs; cat 9202 | ./tojs > $@
digit.js: digit.hs; hastec $<

clean:; -rm $(OUTFILES) *.o *.hi
