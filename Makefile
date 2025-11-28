.DEFAULT_GOAL := all

.PHONY: dependency build run compile-generated nightly all report

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

# Run the full pipeline and produce the HTML speedup report into output/.
report: run
	python3 tools/generate_speedup_index.py --input eval_steps.json --plot output/speedup.png --output output/index.html

nightly all: dependency build run report
