$ErrorActionPreference = "Stop"
& "git" "clean" "-dXf"
& "git" "pull"
& "stack" "exec" "--" "env" "autoreconf" "-i"
if (!$?) {Exit 1}
ForEach ($c in "ghc-8.8","ghc-8.10","ghc-9.0")
{
	& "stack" "--compiler" "$c" "build"
	if (!$?) {Exit 1}
}
Echo "OK"
