default: build

container-build:
    devcontainer build --workspace-folder .

container-up:
    devcontainer up --workspace-folder .

shell: container-up
    devcontainer exec --workspace-folder . bash

format: container-up
    devcontainer exec --workspace-folder . format

build: container-up
    devcontainer exec --workspace-folder . build-all

fullbuild: container-build format build
