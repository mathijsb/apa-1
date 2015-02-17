CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell symlinks

src/CCO/ExWhile/AG.hs : src/CCO/ExWhile/AG.ag src/CCO/Exwhile/AG/Base.ag
	uuagc -Hdcfws --self -P src/CCO/ExWhile src/CCO/ExWhile/AG.ag

haskell : src/CCO/ExWhile/AG.hs
	cabal configure $(CABAL-CONFIGURE-FLAGS)
	cabal build $(CABAL-BUILD-FLAGS)

symlinks : haskell
	mkdir -p bin
	ln -fs ../dist/build/parse-exwhile/parse-exwhile bin/

clean :
	rm -f src/CCO/ExWhile/AG.hs
	rm -rf dist
	rm -rf bin

repl : haskell
	cabal repl

sandbox:
	cabal sandbox init
	cabal install --dependencies-only

.PHONY : haskell clean sandbox
