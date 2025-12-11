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
	python3 tools/generate_speedup_index.py --input eval_steps_simple.json --plot output/live-simple/speedup.png --output output/live-simple/index.html
	python3 tools/generate_speedup_index.py --input eval_steps_left_to_right.json --plot output/live-left-to-right/speedup.png --output output/live-left-to-right/index.html
	python3 tools/generate_speedup_index.py --input eval_steps_demand_driven.json --plot output/live-demand-driven/speedup.png --output output/live-demand-driven/index.html
	python3 tools/generate_speedup_index.py --input eval_steps_from_hazel.json --plot output/hazel/speedup.png --output output/hazel/index.html
	python3 tools/render_live_index.py --output output/index.html \
		--entry "Simple Benchmark=output/live-simple/index.html" \
		--entry "Left-to-right Benchmark=output/live-left-to-right/index.html" \
		--entry "Demand-driven Benchmark=output/live-demand-driven/index.html" \
		--entry "Hazel Benchmark=output/hazel/index.html"

nightly all: dependency build run report
