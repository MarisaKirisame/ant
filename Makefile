.DEFAULT_GOAL := all

.PHONY: dependency build run profile compile-generated nightly all report

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
	pipx install uv
	PATH="$$(pipx environment --value PIPX_BIN_DIR):$$PATH" uv run ./nightly.py all
