FROM mcr.microsoft.com/devcontainers/base:bookworm

# apt
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y dist-upgrade
RUN apt-get install -y autoconf libgmp-dev zstd

USER vscode

# ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/home/vscode/.ghcup/bin:$PATH
RUN ghcup install cabal
ENV GHC_PLAIN_BAD_VERSIONS="9.4.8 9.6.7"
ENV GHC_PLAIN_VERSIONS="9.8.4 9.10.1 9.12.2"
RUN for V in $GHC_PLAIN_VERSIONS $GHC_PLAIN_BAD_VERSIONS; do ghcup install $V; done

# ghcup-wasm
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
RUN ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
ENV GHC_WASM_VERSIONS="wasm32-wasi-9.8.4.20250206 wasm32-wasi-9.10.1.20250327 wasm32-wasi-9.12.2.20250327"
RUN . /home/vscode/.ghc-wasm/env && for V in $GHC_WASM_VERSIONS; do ghcup install $V -- $CONFIGURE_ARGS; done
RUN echo ". /home/vscode/.ghc-wasm/env" >> /home/vscode/.bashrc

ENV PATH=/workspaces/time/bin:$PATH
