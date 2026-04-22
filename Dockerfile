FROM ghcr.io/astral-sh/uv:0.10.9 AS uv

FROM ocaml/opam:debian-12-ocaml-5.4

USER root

COPY --from=uv /uv /uvx /usr/local/bin/

RUN ln -f /usr/bin/opam-2.4 /usr/bin/opam && \
    opam --version | grep -Fx 2.4.1 && \
    uv --version | grep -Fx "uv 0.10.9"

USER opam
WORKDIR /workspace

ENV UV_PYTHON=3.12.13
ENV UV_LINK_MODE=copy

RUN opam init --reinit -ni && \
    opam update && \
    opam install -y dune && \
    eval "$(opam env)" && \
    dune --version

RUN opam switch create ant --empty && \
    opam switch set-invariant --switch ant --update-invariant "ocaml>=5.2" -y && \
    opam install --switch ant -y dune && \
    opam exec --switch ant -- ocamlc -version && \
    opam exec --switch ant -- dune --version

ENV PATH="/home/opam/.opam/5.4/bin:${PATH}"

RUN uv python install "${UV_PYTHON}" && \
    uv run --python "${UV_PYTHON}" python --version | grep -Fx "Python ${UV_PYTHON}"

RUN sudo apt-get update && sudo apt-get install -y \
    linux-perf \
    && sudo rm -rf /var/lib/apt/lists/*

RUN sudo mkdir -p /workspace/_build /home/opam/.cache/dune && \
    sudo chown -R opam:opam /workspace /home/opam/.cache

RUN opam install ocamlformat.0.28.1

CMD ["make", "experiment"]
