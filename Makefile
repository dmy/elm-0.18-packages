STATIC_DIR := docs

all:
	npx elm make --output=${STATIC_DIR}/elm.js --optimize src/Main.elm
	npx elm-minify ${STATIC_DIR}/elm.js --replace

watch:
	npx elm-live src/Main.elm -d ${STATIC_DIR} -u -- --output=${STATIC_DIR}/elm.js

clean:
	rm -f ${STATIC_DIR}/elm.js
