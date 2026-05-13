.DEFAULT_GOAL := all

.PHONY: dependency build coverage run profile compile-generated nightly all report experiment hazel-tex arith-tex hazel hazel-report arith arith-report website website-check website-clean website-serve

dependency:
	uv run ./nightly.py dependency

build:
	uv run ./nightly.py build

coverage:
	uv run ./nightly.py coverage

compile-generated:
	uv run ./nightly.py compile-generated

run:
	uv run ./nightly.py run

hazel:
	uv run ./nightly.py hazel

arith:
	uv run ./nightly.py arith

profile:
	uv run ./nightly.py profile

report:
	uv run ./nightly.py report

hazel-report:
	uv run ./nightly.py hazel-report

arith-report:
	uv run ./nightly.py arith-report

experiment:
	uv run ./nightly.py experiment

hazel-tex:
	uv run ./nightly.py hazel-tex

arith-tex:
	uv run ./nightly.py arith-tex

website:
	uv run python ./website/doc.py build

website-check:
	uv run python ./website/doc.py check

website-clean:
	uv run python ./website/doc.py clean

website-serve:
	uv run python ./website/doc.py serve

# Run the full pipeline and produce the HTML speedup report into output/.
nightly all:
	pipx install uv
	PATH="$$(pipx environment --value PIPX_BIN_DIR):$$PATH" uv run ./nightly.py all
