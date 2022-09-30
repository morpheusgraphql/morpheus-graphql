.PHONY = buildDocs build

.DEFAULT_GOAL = build

# make env v="8.2.2"
env: 
	cp config/stack/$(v).yaml  stack.yaml

build:
	stack build --fast --test --bench --no-run-benchmarks --haddock --no-haddock-deps

clean:
	find . -name "*.cabal" -exec rm -rf {} \;
	stack clean

lint: 
	curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .


build-th:
	stack install --ghc-options -ddump-splices

doc:
	stack build --haddock --no-haddock-deps