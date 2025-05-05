FROM mcr.microsoft.com/devcontainers/base:bookworm

# update Debian
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y dist-upgrade

# user
USER vscode
WORKDIR /home/vscode
ENV LC_ALL=en_US.utf-8

# ghcup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/home/vscode/.ghcup/bin:$PATH

# native back-end
ENV GHC_NATIVE_VERSIONS="9.8.4 9.10.1 9.12.2"
ENV GHC_NATIVE_BAD_VERSIONS="9.4.8 9.6.7"
RUN ghcup install cabal --set latest
ENV PATH=/home/vscode/.cabal/bin:$PATH
RUN for V in $GHC_NATIVE_VERSIONS $GHC_NATIVE_BAD_VERSIONS; do ghcup install ghc $V; done
RUN sudo apt-get install -y libgmp-dev

# formatter
RUN ghcup set ghc 9.12.2
RUN cabal install fourmolu-0.18.0.0

# WebAssembly back-end
ENV GHC_WASM_VERSIONS="wasm32-wasi-9.8.4.20250206 wasm32-wasi-9.10.1.20250327 wasm32-wasi-9.12.2.20250327"
RUN sudo apt-get install -y zstd
WORKDIR /home/vscode
RUN curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | SKIP_GHC=1 sh
RUN ghcup config add-release-channel https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/ghcup-wasm-0.0.9.yaml

# MicroHs back-end
WORKDIR /home/vscode
RUN git clone https://github.com/augustss/MicroHs.git --branch stable-7 mhs
WORKDIR /home/vscode/mhs
RUN make minstall
ENV PATH=/home/vscode/.mcabal/bin:$PATH

# build commands
RUN sudo apt-get install -y autoconf
ENV PATH=/workspaces/time/bin:$PATH
ENV TZ="America/Los_Angeles"
