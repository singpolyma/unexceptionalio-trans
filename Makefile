GHCFLAGS=-Wall -fno-warn-tabs -fno-warn-orphans -fno-warn-name-shadowing -XHaskell98
HLINTFLAGS=-XHaskell98 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use string literal' -i 'Use list comprehension' --utf8 -XMultiParamTypeClasses
VERSION=0.5.2

.PHONY: all clean doc install

all: report.html doc dist/build/libHSunexceptionalio-trans-$(VERSION).a dist/unexceptionalio-trans-$(VERSION).tar.gz

install: dist/build/libHSunexceptionalio-trans-$(VERSION).a
	cabal install

report.html: UnexceptionalIO/Trans.hs
	-hlint $(HLINTFLAGS) --report UnexceptionalIO/Trans.hs

doc: dist/doc/html/unexceptionalio-trans/index.html README

README: unexceptionalio-trans.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/unexceptionalio-trans/index.html: dist/setup-config UnexceptionalIO/Trans.hs 
	cabal haddock --hyperlink-source

dist/setup-config: unexceptionalio-trans.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) report.html
	$(RM) -r dist dist-ghc

dist/build/libHSunexceptionalio-trans-$(VERSION).a: unexceptionalio-trans.cabal dist/setup-config UnexceptionalIO/Trans.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/unexceptionalio-trans-$(VERSION).tar.gz: unexceptionalio-trans.cabal dist/setup-config UnexceptionalIO/Trans.hs README
	cabal check
	cabal sdist
