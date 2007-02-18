GHCFLAGS := -Wall -O2 -funbox-strict-fields -ignore-package streamproc-0.0 -i..

% : %.hs
	ghc ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

% : %.lhs
	ghc ${GHCFLAGS} --make $< -o $@
	@rm -f Main.o Main.hi

test:		sp-test
	time ./sp-test --lazy  </usr/share/dict/words >/dev/null
	time ./sp-test --sp    </usr/share/dict/words >/dev/null
	time ./sp-test --sp\'  </usr/share/dict/words >/dev/null

clean::
	@rm -f sp-test contT-io *.o *.hi
