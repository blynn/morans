.PHONY: all clean

OUTFILES=morans tojs samples.js brain.js digit.js

URL= http://yann.lecun.com/exdb/mnist/
INFILES = train-images-idx3-ubyte.gz train-labels-idx1-ubyte.gz t10k-images-idx3-ubyte.gz t10k-labels-idx1-ubyte.gz
INURLS=$(addprefix $(URL), $(INFILES))

all: $(INFILES) $(OUTFILES)

$(INFILES):
	wget $(INURLS)

morans: $(INFILES) morans.hs; ghc -O2 -Wall -rtsopts -threaded -with-rtsopts="-N -s" -fllvm morans.hs
tojs: tojs.hs; ghc -O2 $<
samples.js: morans; ./morans samplesjs > $@
brain.js: 9202 tojs; cat 9202 | ./tojs > $@
digit.js: digit.hs; hastec $<


clean:; -rm $(OUTFILES) *.o *.hi $(INFILES)
