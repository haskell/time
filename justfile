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

# to run this, your (classic) token must have "repo" and "read:packages"
# to upload the container, it must have "write:packages" and you should be a member of the Haskell org.
act:
    act -s GITHUB_TOKEN="$(gh auth token)" -j build-new
