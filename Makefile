PATH := node_modules/.bin:$(PATH)

STATIC=./static
DIST_STATIC=./dist/static
TMP=./dist/tmp

build: npm tmp css js fonts img site

npm:
	npm install

tmp:
	mkdir -p $(TMP)

css:
	mkdir -p $(TMP)/css/
	mkdir -p $(DIST_STATIC)/css
	cat \
		node_modules/bootstrap/dist/css/bootstrap.css \
		node_modules/font-awesome/css/font-awesome.css \
		$(STATIC)/css/fournil.css \
		> $(TMP)/css/fournil.css
	cat \
		node_modules/leaflet/dist/leaflet.css \
		> $(TMP)/css/fournil-map.css
	cp $(TMP)/css/fournil.css $(DIST_STATIC)/css
	csso $(TMP)/css/fournil.css --output $(TMP)/css/fournil.min.css
	cp $(TMP)/css/fournil-map.css $(DIST_STATIC)/css
	csso $(TMP)/css/fournil-map.css --output $(TMP)/css/fournil-map.min.css

js:
	mkdir -p $(TMP)/js/
	mkdir -p $(DIST_STATIC)/js
	cat \
		node_modules/jquery/dist/jquery.js \
		node_modules/popper.js/dist/umd/popper.js \
		node_modules/bootstrap/js/dist/util.js \
		node_modules/bootstrap/js/dist/collapse.js \
		> $(TMP)/js/fournil.js
	cat \
		node_modules/leaflet/dist/leaflet.js \
		$(STATIC)/js/fournil-map.js \
		> $(TMP)/js/fournil-map.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil.min.js $(TMP)/js/fournil.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil-map.min.js $(TMP)/js/fournil-map.js
	cp $(TMP)/js/fournil.min.js $(DIST_STATIC)/js
	cp $(TMP)/js/fournil-map.min.js $(DIST_STATIC)/js

fonts:
	mkdir -p $(DIST_STATIC)/fonts
	cp -R node_modules/font-awesome/fonts/* $(DIST_STATIC)/fonts/

img:
	mkdir -p $(DIST_STATIC)/img
	cp $(STATIC)/img/* $(DIST_STATIC)/img
	cp node_modules/leaflet/dist/images/* $(DIST_STATIC)/img

site:
	hugo

.PHONY: build
