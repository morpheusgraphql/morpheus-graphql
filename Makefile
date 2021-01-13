.PHONY = sync-docs compile

.DEFAULT_GOAL = compile

compile:
	stack install --fast --test

sync-docs:
	{ cat docs/template/index-header.md ; \
	  echo "\n<!-- This file is generated automatically, see Makefile sync-docs -->\n"; \
	  cat README.md; \
	} > docs/index.md

