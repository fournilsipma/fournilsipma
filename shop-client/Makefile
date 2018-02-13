PATH := node_modules/.bin:$(PATH)

build: npm psc-package purescript

npm:
	npm install

psc-package:
	psc-package update

purescript:
	pulp browserify --to dist/fournil-shop-client.js

.PHONY: build
