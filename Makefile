.DEFAULT_GOAL := all

.PHONY: dependency build run compile-generated nightly all

dependency:
	chmod +x ./nightly.py
	./nightly.py dependency

build:
	chmod +x ./nightly.py
	./nightly.py build

compile-generated:
	chmod +x ./nightly.py
	./nightly.py compile-generated

run:
	chmod +x ./nightly.py
	./nightly.py run

nightly all: dependency build run
