default: fullbuild

container:
    devcontainer up --workspace-folder .

shell: container
    devcontainer exec --workspace-folder . bash

fullbuild: container
    devcontainer exec --workspace-folder . bin/fullbuild
