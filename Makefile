.PHONY = buildDocs build

.DEFAULT_GOAL = build

build:
	stack install --fast --test

genDocs:
	{ cat docs/template/index-header.md ; \
	  echo "\n<!-- This file is generated automatically, with command "make genDocs"  -->\n"; \
	  cat README.md; \
	} > docs/index.md

