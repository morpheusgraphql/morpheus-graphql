.PHONY = sync-docs compile

.DEFAULT_GOAL = compile

compile:
	stack install --fast --test

sync-docs:
	cat docs/template/index-header.md README.md > docs/index.md

