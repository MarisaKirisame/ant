.DEFAULT_GOAL := all

.PHONY: dependency build run nightly all

dependency:
	chmod +x ./nightly.sh
	./nightly.sh dependency

build:
	chmod +x ./nightly.sh
	./nightly.sh build

run:
	chmod +x ./nightly.sh
	./nightly.sh run

nightly all: dependency build run
