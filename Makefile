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

samples:
	stack clean morpheus-graphql-code-gen
	stack install --fast --test morpheus-graphql-code-gen
	morpheus build morpheus-graphql-examples-code-gen/src/**/*.gql --root morpheus-graphql-examples-code-gen/src

build-th:
	stack install --ghc-options -ddump-splices

doc:
	stack build --haddock --no-haddock-deps