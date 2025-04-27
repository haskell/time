default: build

container-build:
    devcontainer build --workspace-folder .

container-up:
    devcontainer up --workspace-folder .

shell: container-up
    devcontainer exec --workspace-folder . bash

format: container-up
    devcontainer exec --workspace-folder . format-all

build: container-up
    devcontainer exec --workspace-folder . fullbuild

fullbuild: container-build format build
