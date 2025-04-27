FROM mcr.microsoft.com/devcontainers/base:bookworm

# apt
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y dist-upgrade
RUN apt-get install -y autoconf libgmp-dev zstd python3

USER vscode
WORKDIR /home/vscode

# ghcup
ENV GHC_PLAIN_VERSIONS="9.8.4 9.10.1 9.12.2"
ENV GHC_PLAIN_BAD_VERSIONS="9.4.8 9.6.7"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/home/vscode/.ghcup/bin:$PATH
RUN ghcup install cabal --set latest
RUN for V in $GHC_PLAIN_VERSIONS $GHC_PLAIN_BAD_VERSIONS; do ghcup install $V; done

# ghcup-wasm
ENV GHC_WASM_VERSIONS="wasm32-wasi-9.8.4.20250206 wasm32-wasi-9.10.1.20250327 wasm32-wasi-9.12.2.20250327"
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
RUN ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
RUN . /home/vscode/.ghc-wasm/env && for V in $GHC_WASM_VERSIONS; do ghcup install ghc $V -- $CONFIGURE_ARGS; done
RUN echo ". /home/vscode/.ghc-wasm/env" >> /home/vscode/.bashrc

# javascript
ENV GHC_JS_VERSIONS="javascript-unknown-ghcjs-9.10.0.20240413 javascript-unknown-ghcjs-9.12.1"
RUN ghcup config add-release-channel cross
RUN git clone https://github.com/emscripten-core/emsdk.git
WORKDIR /home/vscode/emsdk
RUN ./emsdk install 3.1.57
RUN ./emsdk activate 3.1.57
RUN . ./emsdk_env.sh && emconfigure ghcup install ghc javascript-unknown-ghcjs-9.10.0.20240413
RUN ./emsdk install 3.1.74
RUN ./emsdk activate 3.1.74
RUN . ./emsdk_env.sh && emconfigure ghcup install ghc javascript-unknown-ghcjs-9.12.1

ENV PATH=/workspaces/time/bin:$PATH
