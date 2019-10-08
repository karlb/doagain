deploy:
	rsync -avz --progress . -e ssh www.wikdict.com:hosts/static.karl.berlin/doagain
