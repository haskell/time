$ErrorActionPreference = "Stop"
& "git" "clean" "-dXf"
if (!$?) {Exit 1}
& "git" "pull"
if (!$?) {Exit 1}
& "stack" "exec" "--" "env" "autoreconf" "-i"
if (!$?) {Exit 1}
& "ghcup" "upgrade"
if (!$?) {Exit 1}
& "ghcup" "install" "cabal" "latest"
if (!$?) {Exit 1}
& "ghcup" "set" "cabal" "latest"
if (!$?) {Exit 1}
ForEach ($c in "ghc-8.8.4","ghc-8.10.7","ghc-9.0.1")
{
    & "ghcup" "install" "ghc" "$c"
    if (!$?) {Exit 1}
    & "ghcup" "set" "ghc" "$c"
    if (!$?) {Exit 1}
    & "cabal" "update"
    if (!$?) {Exit 1}
    & "cabal" "v1-install" "--only-dependencies" "--enable-tests"
    if (!$?) {Exit 1}
    & "cabal" "v1-configure" "--enable-tests"
    if (!$?) {Exit 1}
    & "cabal" "v1-test"
    if (!$?) {Exit 1}
}
Echo "OK"
