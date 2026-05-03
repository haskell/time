# time

`time` is the Haskell library for clocks, calendars, time zones, local
time, and formatting/parsing of time values. It is bundled with GHC and is
also maintained as the `time` package on Hackage.

The package is configured with Cabal and uses a generated C header. When
building directly from a git checkout, run `autoreconf -i` before the first
Cabal configure/build step if `configure` or the autoconf files are missing
or stale. The devcontainer build scripts do this for normal builds.

## Repository Layout

- `lib/`: library modules exposed through `Data.Time` and internal modules.
- `test/main/`: main Tasty/QuickCheck/HUnit test suite.
- `test/template/`: Template Haskell test suite.
- `test/unix/`: Unix-specific tests.
- `test/`: small standalone test executables.
- `benchmark/`: Criterion benchmark executable.
- `bin/`: build helper scripts used by the devcontainer.
- `.devcontainer/`: full build environment with supported GHC targets.
- `time.cabal`: package metadata, module lists, test suites, and dependencies.
- `cabal.project`: local Cabal project file.
- `justfile`: convenience commands for the devcontainer workflow.

## Common Commands

The primary workflow uses the devcontainer:

```sh
just shell
just format
just build
just haddock
```

`just build` runs `bin/build-all` in the devcontainer, which exercises the
native, expired-native, WebAssembly, JavaScript, and MicroHs build paths
configured there.

For focused local work, Cabal commands are useful when a suitable GHC and
Cabal are on `PATH`:

```sh
autoreconf -i
cabal test test-main
cabal test test-main --test-options '-p /CalendarDiffTime/'
cabal repl
cabal haddock
```

If dependencies have not been fetched yet, run:

```sh
cabal update
```

## Testing

The main test suite is `test-main`. Its entrypoint is
`test/main/Main.hs`, and most reusable test support lives under
`test/main/Test`.

Useful focused test patterns:

```sh
cabal test test-main --test-options '-p /CalendarDiffDays/'
cabal test test-main --test-options '-p /CalendarDiffTime/'
cabal test test-main --test-options '-p /ISO8601/'
```

When adding a new test module under `test/main/Test`, add it to the
`other-modules` list for `test-suite test-main` in `time.cabal`.

## Formatting And Style

The project uses `fourmolu.yaml` for formatting configuration. The
devcontainer installs Fourmolu and exposes it through:

```sh
just format
```

The Cabal file enables `-Wall` and `-fwarn-tabs`; avoid tabs and keep
warnings clean.

## Supported Compilers

The Cabal metadata currently lists these supported GHC versions:

- GHC 9.8.4
- GHC 9.10.3
- GHC 9.12.4
- GHC 9.14.1

The devcontainer also includes older expired-native targets and cross
targets used by the full build.

## Links

- Homepage: <https://github.com/haskell/time>
- Issues: <https://github.com/haskell/time/issues>
- GHC: <https://www.haskell.org/ghc/>
