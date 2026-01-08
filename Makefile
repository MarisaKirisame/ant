.DEFAULT_GOAL := all

.PHONY: dependency build run profile compile-generated nightly all report temp

dependency:
	./nightly.py dependency

build:
	./nightly.py build

compile-generated:
	./nightly.py compile-generated

run:
	./nightly.py run

profile:
	./nightly.py profile

report:
	./nightly.py report

# Run the full pipeline and produce the HTML speedup report into output/.
nightly all:
	./nightly.py all

temp:
	opam exec --switch ant -- dune exec ant -- examples/Test.ant generated/TestCEK.ml --compile --backend memo
	opam exec --switch ant -- dune exec ant -- examples/Test.ant generated/TestRegCEK.ml --compile --backend reg-memo
	opam exec --switch ant -- dune fmt --display=quiet > /dev/null 2>&1 || true

