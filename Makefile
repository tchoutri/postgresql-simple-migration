package = postgresql-migration
exe = migrate
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

stack-check-nightly:
	$(stack) setup --resolver nightly
	$(stack) build --resolver nightly --pedantic --test

stack-build:
	$(stack) build $(package) --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-fast:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-watch:
	$(stack) build $(package) --fast --file-watch --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

stack-run:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS" && $(stack) exec -- $(exe)

stack-ghci:
	$(stack) ghci $(package):lib --ghci-options='-j8 +RTS -A128m -n2m -qg'

stack-ghcid:
	$(stack) exec -- ghcid --lint -c "stack ghci $(package):lib --ghci-options='-ignore-dot-ghci -fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(exe)"

stack-ghcid-quiet:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-ignore-dot-ghci -fobject-code -fno-warn-unused-do-bind -fno-warn-unused-matches -fno-warn-unused-local-binds -fno-warn-unused-imports -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(exe)"

stack-test:
	$(stack) test --ta "psql"



cabal-run:
	cabal run $(exe)

cabal-build:
	cabal build $(package) --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

cabal-build-fast:
	cabal build $(package) --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-ghcid:
	ghcid --lint -c "cabal repl --repl-options='-ignore-dot-ghci' --repl-options='-fobject-code' --repl-options='-fno-warn-unused-do-bind' --repl-options='-j6' "

cabal-test:
	cabal run --test-show-details=direct test:tests psql


.PHONY : stack-build stack-build-dirty stack-run stack-ghci stack-ghcid stack-check-nightly cabal-ghcid cabal-build cabal-run cabal-build-fast
