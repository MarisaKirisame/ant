.DEFAULT_GOAL := all

NIGHTLY_ARGS ?=
DOCKER ?= docker
DOCKER_IMAGE ?= chordata-artifact
ARTIFACT_UID ?= $(shell test "$$(id -u)" -eq 0 && echo 1000 || id -u)
ARTIFACT_GID ?= $(shell test "$$(id -g)" -eq 0 && echo 1000 || id -g)
DOCKER_BUILD_ARGS ?= --build-arg ARTIFACT_UID=$(ARTIFACT_UID) --build-arg ARTIFACT_GID=$(ARTIFACT_GID)
ARTIFACT_RUN_DIR ?= $(CURDIR)/docker-output
AGDA ?= agda
AGDA_STDLIB ?= /opt/agda/lib/agda-stdlib-2.2/src

.PHONY: smoke quick eval proof dependency hazel-dependency build coverage run profile compile-generated nightly all report experiment hazel-experiment hazel-no-evict hazel-tex arith-tex hazel hazel-report arith arith-report arith-scaling hazel-scaling hazel-no-evict-scaling scaling scaling-report entropy-scaling entropy-report proof-native website website-check website-clean website-serve docker-build docker-shell

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

proof-native:
	$(AGDA) -i proof -i "$(AGDA_STDLIB)" proof/all.agda

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

docker-build:
	$(DOCKER) build $(DOCKER_BUILD_ARGS) -t $(DOCKER_IMAGE) .

smoke: docker-build
	mkdir -p "$(ARTIFACT_RUN_DIR)/results" "$(ARTIFACT_RUN_DIR)/output"
	test -n "$(ARTIFACT_RUN_DIR)"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "/"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "$(HOME)"
	find "$(ARTIFACT_RUN_DIR)/results" -mindepth 1 -delete
	find "$(ARTIFACT_RUN_DIR)/output" -mindepth 1 -delete
	$(DOCKER) run --rm --network none \
		-v "$(ARTIFACT_RUN_DIR)/results:/workspace/results" \
		-v "$(ARTIFACT_RUN_DIR)/output:/workspace/output" \
		$(DOCKER_IMAGE) make nightly "NIGHTLY_ARGS=--smoke --skip-dependencies"

quick: docker-build
	mkdir -p "$(ARTIFACT_RUN_DIR)/results" "$(ARTIFACT_RUN_DIR)/output"
	test -n "$(ARTIFACT_RUN_DIR)"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "/"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "$(HOME)"
	find "$(ARTIFACT_RUN_DIR)/results" -mindepth 1 -delete
	find "$(ARTIFACT_RUN_DIR)/output" -mindepth 1 -delete
	$(DOCKER) run --rm --network none \
		-v "$(ARTIFACT_RUN_DIR)/results:/workspace/results" \
		-v "$(ARTIFACT_RUN_DIR)/output:/workspace/output" \
		$(DOCKER_IMAGE) make hazel
	$(DOCKER) run --rm --network none \
		-v "$(ARTIFACT_RUN_DIR)/results:/workspace/results" \
		-v "$(ARTIFACT_RUN_DIR)/output:/workspace/output" \
		$(DOCKER_IMAGE) make hazel-report

eval: docker-build
	mkdir -p "$(ARTIFACT_RUN_DIR)/results" "$(ARTIFACT_RUN_DIR)/output"
	test -n "$(ARTIFACT_RUN_DIR)"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "/"
	test "$(abspath $(ARTIFACT_RUN_DIR))" != "$(HOME)"
	find "$(ARTIFACT_RUN_DIR)/results" -mindepth 1 -delete
	find "$(ARTIFACT_RUN_DIR)/output" -mindepth 1 -delete
	$(DOCKER) run --rm --network none \
		-v "$(ARTIFACT_RUN_DIR)/results:/workspace/results" \
		-v "$(ARTIFACT_RUN_DIR)/output:/workspace/output" \
		$(DOCKER_IMAGE) make nightly NIGHTLY_ARGS=--skip-dependencies

proof: docker-build
	$(DOCKER) run --rm --network none $(DOCKER_IMAGE) make proof-native

docker-shell: docker-build
	$(DOCKER) run --rm -it --network none $(DOCKER_IMAGE) bash
