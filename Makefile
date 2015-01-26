default: clean test sdist

# Building

clean:
	cabal clean
	cd test && make clean

configure:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests

build: configure
	cabal build --ghc-options=-Werror

haddock: configure
	cabal haddock

copy: build test haddock
	cabal copy

install:
	cabal install --user --ghc-options=-Werror --enable-library-profiling --enable-executable-profiling

sdist: clean configure
	cabal sdist

test: install
	cd test && make clean && make

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default clean configure build haddock copy install test sdist
