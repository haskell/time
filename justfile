default: fullbuild

container-build:
    devcontainer --workspace-folder . build

container-up:
    devcontainer --workspace-folder . up

shell: container-up
    devcontainer --workspace-folder . exec bash

fullbuild: container-up
    devcontainer --workspace-folder . exec fullbuild
