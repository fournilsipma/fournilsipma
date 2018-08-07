PATH := $(shell npm bin):$(PATH)

build: npm psc-package purescript

npm:
	npm install

psc-package:
	psc-package install

purescript:
	pulp browserify --to dist/fournil-shop-client.js

.PHONY: build
