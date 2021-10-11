$ErrorActionPreference = "Stop"
& "git" "clean" "-dXf"
& "git" "pull"
& "stack" "exec" "--" "env" "autoreconf" "-i"
if (!$?) {Exit 1}
ForEach ($c in "ghc-8.8.4","ghc-8.10.7","ghc-9.0.1")
{
	& "stack" "--compiler" "$c" "build"
	if (!$?) {Exit 1}
}
Echo "OK"
