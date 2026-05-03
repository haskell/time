FROM mcr.microsoft.com/devcontainers/base:bookworm

# Update this to force full rebuild
RUN echo "2026-04-27" > /container.date

# update Debian
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y dist-upgrade

# user
USER vscode
WORKDIR /home/vscode
ENV LC_ALL=en_US.utf-8

# ghcup
ARG BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ARG BOOTSTRAP_HASKELL_MINIMAL=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/home/vscode/.ghcup/bin:$PATH

# cabal
RUN ghcup install cabal --set latest
RUN cabal update
ENV PATH=/home/vscode/.cabal/bin:$PATH

# native back-end
ENV GHC_NATIVE_VERSIONS="9.8.4 9.10.3 9.12.4 9.14.1"
ENV GHC_NATIVE_EXPIRED_VERSIONS="9.4.8 9.6.7"
WORKDIR /home/vscode
RUN for V in $GHC_NATIVE_VERSIONS $GHC_NATIVE_EXPIRED_VERSIONS; do ghcup install ghc $V; done
RUN sudo apt-get install -y libgmp-dev

# formatter
WORKDIR /home/vscode
RUN ghcup set ghc 9.12.4
RUN cabal install fourmolu-0.19.0.1

# WebAssembly back-end
ENV GHC_WASM32_VERSIONS="wasm32-wasi-9.8 wasm32-wasi-9.10 wasm32-wasi-9.12 wasm32-wasi-9.14"
WORKDIR /home/vscode
RUN sudo apt-get install -y zstd
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
RUN ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml
RUN . /home/vscode/.ghc-wasm/env && for V in $GHC_WASM32_VERSIONS; do ghcup install ghc $V -- $CONFIGURE_ARGS; done
RUN curl -LO https://github.com/bytecodealliance/wasmtime/releases/download/dev/wasmtime-dev-x86_64-linux.tar.xz
RUN xz -d wasmtime-dev-x86_64-linux.tar.xz
RUN tar xvf wasmtime-dev-x86_64-linux.tar
ENV PATH=/home/vscode/wasmtime-dev-x86_64-linux:$PATH

# JavaScript back-end
ENV GHC_JS_VERSIONS="javascript-unknown-ghcjs-9.10 javascript-unknown-ghcjs-9.12"
WORKDIR /home/vscode
RUN sudo apt-get install -y nodejs
RUN ghcup config add-release-channel cross
RUN git clone https://github.com/emscripten-core/emsdk.git
WORKDIR /home/vscode/emsdk
RUN sudo apt-get install -y python3
RUN ./emsdk install 3.1.74
RUN ./emsdk activate 3.1.74
RUN . ./emsdk_env.sh && emconfigure ghcup install ghc javascript-unknown-ghcjs-9.10
RUN . ./emsdk_env.sh && emconfigure ghcup install ghc javascript-unknown-ghcjs-9.12

# MicroHs back-end
WORKDIR /home/vscode
RUN git clone https://github.com/augustss/MicroHs.git --branch v0.15.4.0 mhs
WORKDIR /home/vscode/mhs
RUN make minstall
ENV PATH=/home/vscode/.mcabal/bin:$PATH

# build commands
WORKDIR /home/vscode
RUN sudo apt-get install -y autoconf
ENV PATH=/workspaces/time/bin:$PATH
ENV TZ="America/Los_Angeles"
