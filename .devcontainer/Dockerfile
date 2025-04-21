FROM mcr.microsoft.com/devcontainers/base:bookworm

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y dist-upgrade
RUN apt-get install -y autoconf libgmp-dev

USER vscode
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/home/vscode/.ghcup/bin:$PATH
RUN ghcup install cabal
RUN ghcup install ghc 9.8.4
RUN ghcup install ghc 9.10.1
RUN ghcup install ghc 9.12.2
