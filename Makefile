all:
	sh doc/drawdata.sh > doc/raml.dot
	cat doc/raml.dot | dot -Tpng > doc/raml.png

devel:
	commando -p cat -q -j | grep --line-buffered 'hs$$' | uniqhash | conscript make
