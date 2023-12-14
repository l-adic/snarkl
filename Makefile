Snarkl.Toplevel:
	cabal sandbox init; \
	cabal install

snarky: 
	cd cppsrc; \
	make

test: Snarkl.Toplevel snarky
	cabal test

bench: Snarkl.Toplevel snarky
	cabal bench 2> /dev/null

clean:
	cd cppsrc; \
	make clean; \
	cd ..; \
	cabal clean

clean-all: clean
	rm -rf depsrc; \
	rm -rf depinst; \
	cabal sandbox delete

.PHONY: Snarkl.Toplevel snarky test bench clean clean-all
