FROM ghcr.io/astral-sh/uv:0.11.21 AS uv

FROM ltchentw/agda:2.7.0.1-no-emacs@sha256:4c0e7c2b742ade59df60bae50a4256db6ef16b9aab9131e0615d726d1f0946b4 AS agda

FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG ARTIFACT_UID=1000
ARG ARTIFACT_GID=1000

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       build-essential \
       ca-certificates \
       curl \
       git \
       libffi-dev \
       libgmp-dev \
       libzstd-dev \
       m4 \
       nodejs \
       npm \
       opam \
       pkg-config \
       python3 \
       python3-dev \
       python3-venv \
       rsync \
       tini \
       unzip \
       zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY --from=uv /uv /uvx /usr/local/bin/
COPY --from=agda /opt/agda /opt/agda

RUN existing_user="$(getent passwd "${ARTIFACT_UID}" | cut -d: -f1 || true)" \
    && if [ -n "${existing_user}" ]; then userdel --remove "${existing_user}"; fi \
    && existing_group="$(getent group "${ARTIFACT_GID}" | cut -d: -f1 || true)" \
    && if [ -n "${existing_group}" ]; then groupdel "${existing_group}"; fi \
    && groupadd --gid "${ARTIFACT_GID}" artifact \
    && useradd \
       --create-home \
       --gid "${ARTIFACT_GID}" \
       --shell /bin/bash \
       --uid "${ARTIFACT_UID}" \
       artifact

ENV OPAMROOT=/home/artifact/.opam \
    OPAMYES=1 \
    UV_CACHE_DIR=/home/artifact/.cache/uv \
    MPLCONFIGDIR=/home/artifact/.cache/matplotlib \
    AGDA_STDLIB=/opt/agda/lib/agda-stdlib-2.2/src \
    Agda_datadir=/opt/agda/data \
    PATH=/opt/agda/bin:${PATH} \
    PYTHONUNBUFFERED=1

WORKDIR /workspace
COPY --chown=artifact:artifact . /workspace
RUN chown artifact:artifact /workspace

USER artifact

RUN opam init --bare --disable-sandboxing --no-setup \
    && uv sync --frozen

RUN uv run ./nightly.py dependency --skip-dev-tools
RUN uv run ./nightly.py hazel-dependency
RUN uv run ./nightly.py build \
    && opam exec --switch ant -- dune runtest \
    && rm -rf "${UV_CACHE_DIR}" "${MPLCONFIGDIR}"

USER root
RUN make proof-native \
    && find proof -name '*.agdai' -delete

USER artifact

ENTRYPOINT ["/usr/bin/tini", "--", "/workspace/docker/entrypoint.sh"]
CMD ["make", "nightly", "NIGHTLY_ARGS=--smoke --skip-dependencies"]
