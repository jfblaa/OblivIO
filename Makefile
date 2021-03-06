.PHONY: default deps clean report-missing-deps
BUILD=dune build

native:
	$(BUILD) _build/install/default/bin/oblivio

all:
	$(BUILD)

deps:
	opam install --deps-only .

clean:
	dune clean

report-missing-deps:
	dune external-lib-deps @install --missing

utop:
	dune utop . -- -init=./.ocamlinit
