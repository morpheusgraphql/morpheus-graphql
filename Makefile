.PHONY = buildDocs build

.DEFAULT_GOAL = build


env: 
	cp config/stack/$(v).yaml  stack.yaml

build:
	stack install --fast --test

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
