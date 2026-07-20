.DEFAULT_GOAL := all

NIGHTLY_ARGS ?=

.PHONY: dependency hazel-dependency build coverage run profile compile-generated nightly all report experiment hazel-experiment hazel-core-experiment hazel-no-evict hazel-tex arith-tex hazel hazel-report arith arith-report arith-scaling hazel-scaling hazel-no-evict-scaling scaling scaling-report entropy-scaling entropy-report website website-check website-clean website-serve

dependency:
	uv run ./nightly.py dependency

hazel-dependency:
	uv run ./nightly.py hazel-dependency

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

hazel-experiment:
	uv run ./nightly.py hazel-experiment

hazel-core-experiment:
	uv run ./nightly.py hazel-core-experiment

hazel-no-evict:
	uv run ./nightly.py hazel-no-evict

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

arith-scaling:
	uv run ./nightly.py arith-scaling

hazel-scaling:
	uv run ./nightly.py hazel-scaling

hazel-no-evict-scaling:
	uv run ./nightly.py hazel-no-evict-scaling

scaling:
	uv run ./nightly.py scaling

scaling-report:
	uv run ./nightly.py scaling-report

entropy-scaling:
	uv run ./nightly.py entropy-scaling

entropy-report:
	uv run ./nightly.py entropy-report

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
	UV_BIN="$$(command -v uv || true)"; \
	if [ -z "$$UV_BIN" ]; then \
		PIPX_BIN_DIR="$$(pipx environment --value PIPX_BIN_DIR)"; \
		if [ ! -x "$$PIPX_BIN_DIR/uv" ]; then pipx install uv; fi; \
		UV_BIN="$$PIPX_BIN_DIR/uv"; \
	fi; \
	"$$UV_BIN" run ./nightly.py all $(NIGHTLY_ARGS)
