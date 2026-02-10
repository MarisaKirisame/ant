.DEFAULT_GOAL := all

.PHONY: dependency build run profile compile-generated nightly all report temp

dependency:
	uv run ./nightly.py dependency

build:
	uv run ./nightly.py build

compile-generated:
	uv run ./nightly.py compile-generated

run:
	uv run ./nightly.py run

profile:
	uv run ./nightly.py profile

report:
	uv run ./nightly.py report

# Run the full pipeline and produce the HTML speedup report into output/.
nightly all:
	uv run ./nightly.py all

temp:
	opam exec --switch ant -- dune exec ant -- examples/Test.ant generated/TestCEK.ml --compile --backend memo
	opam exec --switch ant -- dune exec ant -- examples/Test.ant generated/TestRegCEK.ml --compile --backend reg-memo
	opam exec --switch ant -- dune fmt --display=quiet > /dev/null 2>&1 || true
