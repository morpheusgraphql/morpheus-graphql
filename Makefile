.PHONY = buildDocs build

.DEFAULT_GOAL = build

build:
	stack build --fast --test --bench --no-run-benchmarks --haddock --no-haddock-deps

clean:
	find . -name "*.cabal" -exec rm -rf {} \;
	stack clean

build-th:
	stack install --ghc-options -ddump-splices

doc:
	stack build --haddock --no-haddock-deps