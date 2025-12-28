.DEFAULT_GOAL := all

.PHONY: dependency build run profile compile-generated nightly all report

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
