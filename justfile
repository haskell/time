default: fullbuild

container:
    devcontainer up --workspace-folder .

fullbuild: container
    devcontainer exec --workspace-folder . bin/fullbuild
