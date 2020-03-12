$ErrorActionPreference = "Stop"
ForEach ($r in "lts-9","lts-11","lts-12","lts-14","lts-15")
{
	& "stack" "--resolver" "$r" "build"
	if (!$?) {Exit 1}
}
Echo "OK"
