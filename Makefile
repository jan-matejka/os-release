.PHONY: all bench build clean configure haddock install repl run test

all: install configure build haddock test bench

bench:
	cabal bench

build:
	cabal build

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi

configure:
	cabal configure --enable-benchmarks --enable-tests

haddock:
	cabal haddock --hyperlink-source

install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --only-dependencies --reorder-goals

repl:
	cabal repl lib:NCommander

run:
	cabal run NCommander

test:
	cabal test
