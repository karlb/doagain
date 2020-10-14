build:
	elm make src/Main.elm --output=elm.js

format:
	npx elm-format src/*

deploy:
	rsync -avz --progress . -e ssh www.wikdict.com:hosts/static.karl.berlin/doagain
