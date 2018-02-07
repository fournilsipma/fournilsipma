PATH := node_modules/.bin:$(PATH)

STATIC=./static
DIST_STATIC=./dist/static

build: npm css js fonts img site

npm:
	npm install

css:
	mkdir -p $(DIST_STATIC)/css && cp node_modules/bootstrap/dist/css/bootstrap.min.css $(DIST_STATIC)/css/
	cp node_modules/leaflet/dist/leaflet.css $(DIST_STATIC)/css/
	cp $(STATIC)/css/* $(DIST_STATIC)/css

js:
	mkdir -p $(DIST_STATIC)/js && cp node_modules/leaflet/dist/leaflet.js $(DIST_STATIC)/js/
	cp node_modules/jquery/dist/jquery.min.js $(DIST_STATIC)/js/
	cp node_modules/popper.js/dist/umd/popper.min.js $(DIST_STATIC)/js/
	cp node_modules/bootstrap/js/dist/util.js $(DIST_STATIC)/js/
	cp node_modules/bootstrap/js/dist/collapse.js $(DIST_STATIC)/js/
	cp $(STATIC)/js/* $(DIST_STATIC)/js

fonts:
	mkdir -p $(DIST_STATIC)/fonts && cp -R node_modules/font-awesome/fonts/* $(DIST_STATIC)/fonts/
	mkdir -p $(DIST_STATIC)/css && cp -R node_modules/font-awesome/css/font-awesome.min.css $(DIST_STATIC)/css/

img:
	mkdir -p $(DIST_STATIC)/img && cp $(STATIC)/img/* $(DIST_STATIC)/img
	cp node_modules/leaflet/dist/images/* $(DIST_STATIC)/img

site:
	hugo

.PHONY: build
