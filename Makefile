PATH := node_modules/.bin:$(PATH)
DIST=./docs

build: npm css fonts

npm:
	npm install

css:
	mkdir -p $(DIST)/css && cp node_modules/bootstrap/dist/css/bootstrap.min.css $(DIST)/css/

fonts:
	mkdir -p $(DIST)/fonts && cp -R node_modules/font-awesome/fonts/* $(DIST)/fonts/
	mkdir -p $(DIST)/css && cp -R node_modules/font-awesome/css/font-awesome.min.css $(DIST)/css/

.PHONY: build
