PATH := $(shell npm bin):$(PATH)

STATIC=./static
DIST_STATIC=./dist/static
TMP=./dist/tmp

build: npm css js fonts img pdf data shopclient site

npm:
	npm install

css: npm
	mkdir -p $(TMP)/css/
	mkdir -p $(DIST_STATIC)/css
	sass $(STATIC)/scss/fournil.scss > $(TMP)/css/fournil-base.css
	awk 1 \
		node_modules/bootstrap/dist/css/bootstrap.css \
		node_modules/font-awesome/css/font-awesome.css \
		$(TMP)/css/fournil-base.css \
		> $(TMP)/css/fournil.css
	csso $(TMP)/css/fournil.css --output $(TMP)/css/fournil.min.css
	cp $(TMP)/css/fournil.min.css $(DIST_STATIC)/css/
	awk 1 \
		node_modules/leaflet/dist/leaflet.css \
		> $(TMP)/css/fournil-map.css
	csso $(TMP)/css/fournil-map.css --output $(TMP)/css/fournil-map.min.css
	cp $(TMP)/css/fournil-map.min.css $(DIST_STATIC)/css/

js: npm
	mkdir -p $(TMP)/js/
	mkdir -p $(DIST_STATIC)/js
	awk 1 \
		node_modules/jquery/dist/jquery.js \
		node_modules/popper.js/dist/umd/popper.js \
		node_modules/bootstrap/dist/js/bootstrap.js \
		> $(TMP)/js/fournil.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil.min.js $(TMP)/js/fournil.js
	cp $(TMP)/js/fournil.min.js $(DIST_STATIC)/js
	awk 1 \
		node_modules/leaflet/dist/leaflet.js \
		$(STATIC)/js/fournil-map.js \
		> $(TMP)/js/fournil-map.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil-map.min.js $(TMP)/js/fournil-map.js
	cp $(TMP)/js/fournil-map.min.js $(DIST_STATIC)/js
	awk 1 \
		node_modules/moment/moment.js \
		node_modules/moment/locale/fr.js \
		node_modules/moment-range/dist/moment-range.js \
		$(STATIC)/js/fournil-holidays.js \
		> $(TMP)/js/fournil-holidays.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil-holidays.min.js $(TMP)/js/fournil-holidays.js
	cp $(TMP)/js/fournil-holidays.min.js $(DIST_STATIC)/js

fonts:
	mkdir -p $(DIST_STATIC)/fonts
	cp -R $(STATIC)/fonts/* $(DIST_STATIC)/fonts/
	cp -R node_modules/font-awesome/fonts/* $(DIST_STATIC)/fonts/

img:
	mkdir -p $(DIST_STATIC)/img
	cp $(STATIC)/img/* $(DIST_STATIC)/img/
	cp node_modules/leaflet/dist/images/* $(DIST_STATIC)/img/

pdf:
	mkdir -p $(DIST_STATIC)/pdf
	cp $(STATIC)/pdf/* $(DIST_STATIC)/pdf/

data: npm
	mkdir -p $(DIST_STATIC)
	yaml2json data/fournil.yaml >$(DIST_STATIC)/fournil.json

shopclient: npm
	cd shop-client && make
	awk 1 \
		node_modules/moment/moment.js \
		node_modules/moment-range/dist/moment-range.js \
		node_modules/pikaday/pikaday.js \
		shop-client/dist/fournil-shop-client.js \
		> $(TMP)/js/fournil-shop-client.js
	uglifyjs --compress --mangle -o $(TMP)/js/fournil-shop-client.min.js $(TMP)/js/fournil-shop-client.js
	cp $(TMP)/js/fournil-shop-client.min.js $(DIST_STATIC)/js
	awk 1 \
		node_modules/pikaday/css/pikaday.css \
		> $(TMP)/css/fournil-shop-client.css
	csso $(TMP)/css/fournil-shop-client.css --output $(TMP)/css/fournil-shop-client.min.css
	cp $(TMP)/css/fournil-shop-client.min.css $(DIST_STATIC)/css/

site:
	hugo

.PHONY: build
